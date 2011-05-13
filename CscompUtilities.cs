// CscompUtilities.cs
// ------------------------------------------------------------------
//
// Author: Dinoch
// built on host: DINOCH-2
// Created Mon Apr 21 08:40:47 2008
//
// Last Saved: <2011-May-12 15:19:19>
//
//
// This file defines code for an assembly containing one main class, a
// static class that exposes only static methods.  The assembly is
// intended to run within Powershell, in an emacs inferior shell.
//
// Using csharp-complete.el, when the user asks for code-completion on a
// segment of code, csharp-complete will send a command in the
// powershell - which just invokes a method on this static
// class.
//
// The logic in this assembly will then perform whatever is necessary:
// reflect on the specified type, or qualify a name, and so on, and then
// return the result information to the Csharp Completion elisp logic.
//
// In broad strokes, you can think of this assembly as the thing that
// performs .NET reflection, and sends the list of potential completions
// to elisp, which presents a pop-up menu. There are a bunch of
// supplementary tasks required, in order to make the "return the list
// of potential completions" possible: for example, is the completion
// being requested on a type?  A namespace?  is it a static method?  A
// property? and so on.  All of these extra supporting functions are also
// implemented as static methods on the main Utilities class.
//
// =======================================================
//
// compile with:
//   csc.exe  /target:library  /debug /out:CscompUtilities.dll  CscompUtilities.cs
//
// ------------------------------------------------------------------
//
// Copyright (c) 2008-2011 by Dino Chiesa
// All rights reserved!
//
// ------------------------------------------------------------------




using System;
using System.IO;
using System.Linq;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Reflection;
using ICSharpCode.NRefactory;
using ICSharpCode.NRefactory.Ast;

// to allow fast ngen
[assembly: AssemblyTitle("CscompUtilities.cs")]
[assembly: AssemblyDescription("an assembly to be loaded into powershell, allows integration with emacs, code completion, etc.")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("Dino Chiesa")]
[assembly: AssemblyProduct("Tools")]
[assembly: AssemblyCopyright("Copyright © Dino Chiesa 2010, 2011")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("1.3.0.2")]


namespace Ionic.Cscomp
{
    public static class Utilities
    {
        private static List<string> StarterAssemblies = new List<string>()
        {
            "System, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089",
            "mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
            // {"System.Xml","Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" },
            // {"System.Xml.Linq", "Version=3.5.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" },
            // {"System.Data", "Version=3.5.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" },
        };


        //             "Microsoft.JScript",
        //             "Microsoft.VisualBasic",
        //             "Microsoft.VisualBasic.Vsa",
        //             "Microsoft.VisualC",
        //             "Microsoft.Vsa",
        //             "Microsoft.Vsa.Vb.CodeDOMPRocessor",
        //             "System.Configuration.Install",
        //             "System.Data",
        //             "System.Design",
        //             "System.DirectoryServices",
        //             "System.Drawing",
        //             "System.Drawing.Design",
        //             "System.EnterpriseServices",
        //             "System.Management",
        //             "System.Messaging",
        //             "System.Runtime.Remoting",
        //             "System.Runtime.Serialization.Formatters.Soap",
        //             "System.Security",
        //             "System.ServiceProcess",
        //             "System.Web",
        //             "System.Web.RegularExpressions",
        //             "System.Web.Services",
        //             "System.Windows.Forms",

        private static List<String>       _GacAssemblies;
        private static Dictionary<String,Object>       _assembliesNotLoaded;
        private static Dictionary<String,Assembly>     _assembliesLoaded;
        private static Dictionary<String,String>       _assemblyForType;
        private static Dictionary<String,List<String>> _typesForNamespace;
        private static Dictionary<String,String>       _fullNamesForShortNames;
        private static List<String>                    _SearchPaths;

        static Utilities()
        {
            try
            {
                SetBasicSearchPaths();
                ReadGac(false);
                LoadAssembliesAndPopulateHashes();
            }
            catch (System.Exception exc1)
            {
                System.Console.WriteLine("uncaught exception {0}", exc1);
            }
        }

        public static string ReadGac(bool wantList)
        {
            if (_GacAssemblies == null)
            {
                var p = new System.Diagnostics.Process
                    {
                        StartInfo =
                        {
                            FileName = "gacutil.exe",
                            CreateNoWindow = true,
                            Arguments = "-l",
                            RedirectStandardOutput = true,
                            //RedirectStandardError = true,
                            WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden,
                            UseShellExecute = false,
                        }
                    };

                p.Start();
                _GacAssemblies = new List<String>();

                string output = p.StandardOutput.ReadToEnd();
                string[] lines = output.Split("\n".ToCharArray());
                foreach (var line in lines)
                {
                    var mcol= Regex.Matches(line,"^([^,]+), *(.+)");
                    foreach (Match match in mcol)
                    {
                        // var p1 = match.Groups[1].Value;
                        // var p2 = match.Groups[2].Value;
                        // System.Console.WriteLine("{0}, {1}", p1, p2);
                        _GacAssemblies.Add(line.Trim());
                    }
                }

            }

            if (!wantList) return "t";

            string atoms = String.Join("\" \"", _GacAssemblies.ToArray());
            return "(list \"" + atoms + "\")";
        }


        // private static string ExpandEnvVarsInPath(string path)
        // {
        //     bool done;
        //     do
        //     {
        //         done= true;
        //         Match match = Regex.Match(path,"%([^%]+)%");
        //         if (match.Success)
        //         {
        //             done= false;
        //             var envvar = match.Groups[1].Value.ToString();
        //             var value = System.Environment.GetEnvironmentVariable(envvar);
        //             path = path.Replace("%"+envvar+"%", value);
        //         }
        //     } while (!done);
        //     return path;
        // }



        private static void SetBasicSearchPaths()
        {
            _SearchPaths= new List<String>();

            Microsoft.Win32.RegistryKey rkey=
                Microsoft.Win32.Registry.LocalMachine.OpenSubKey
                ("SOFTWARE\\Microsoft\\.NETFramework", false);

            String DotNetDir= (string) rkey.GetValue("InstallRoot");
            string programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);

            foreach (var path in new String[] {
                    Path.Combine(DotNetDir,"v2.0.50727"),

                    Path.Combine(programFiles,"Reference Assemblies\\Microsoft\\Framework\\v3.5"),
                    Path.Combine(programFiles,"Reference Assemblies\\Microsoft\\Framework\\v3.0") } )
            {
                if (Directory.Exists(path))
                {
                    Tracing.Trace("SetBasicSearchPaths: {0}", path);
                    _SearchPaths.Add(path);
                }
            }
        }


        // private static Assembly LoadRefAssembly(string fileName)
        // {
        //     var fullPath = FindAssemblyInSearchPaths(fileName);
        //     if (fullPath != null)
        //         return Assembly.LoadFrom(fullPath) ;
        // }




        private static Assembly AssemblyIsLoaded(string assemblyName)
        {
            // exact match
            if (_assembliesLoaded.Keys.Contains(assemblyName))
                return _assembliesLoaded[assemblyName];

            // check for short name
            if (!assemblyName.Contains(","))
            {
                foreach (var key in _assembliesLoaded.Keys)
                {
                    int ix = key.LastIndexOf(',');
                    if (ix > 0)
                    {
                        var stub = key.Substring(0, ix);
                        if (assemblyName == stub)
                            return _assembliesLoaded[key];
                    }
                }
            }
            return null;
        }



        private static String InternalLoadOneAssembly(string assemblyName)
        {
            // assemblyName can be a path, or a fully-qualified name, or
            // a partially qualified name.  We need to map all those to
            // the FQ name.
            Tracing.Trace("InternalLoadOneAssembly '{0}'", assemblyName);

            if (String.IsNullOrEmpty(assemblyName))
            {
                Tracing.Trace("InternalLoadOneAssembly: arg is null, returning null");
                return null;
            }

            // check if already loaeded
            if (AssemblyIsLoaded(assemblyName)!=null)
            {
                Tracing.Trace("InternalLoadOneAssembly: already loaded");
                return "t";
            }

            // maybe not already loaded. Try to load it.
            Assembly thisAssembly = null;
            try
            {
                thisAssembly = TryLoadAssembly(assemblyName);
            }
            catch(Exception exc1)
            {
                _assembliesNotLoaded[assemblyName] = exc1;
                Tracing.Trace("InternalLoadOneAssembly: exception: {0}", exc1);
                return null;
            }

            if (thisAssembly == null)
            {
                _assembliesNotLoaded[assemblyName] = "Assembly was null.";
                Tracing.Trace("InternalLoadOneAssembly: loaded assembly was null");
                return null;
            }

            // ok, we now have an assembly loaded
            string shortName = thisAssembly.FullName.Split(',')[0];

            // check if already loaeded
            if (AssemblyIsLoaded(shortName)!=null)
            {
                Tracing.Trace("InternalLoadOneAssembly: assembly '{0}' already loaded",
                              shortName);
                return "t";
            }

            Tracing.Trace("InternalLoadOneAssembly: loading assembly '{0}'...",
                          thisAssembly.FullName);
            _assembliesLoaded.Add(shortName, thisAssembly);

            Module[] ma = thisAssembly.GetModules();
            if (ma != null)
            {
                List<String> list;

                for (int k = 0; k < ma.Length; k++)
                {
                    try
                    {
                        if (ma[k] == null)  continue;

                        Type[] types = ma[k].GetTypes();
                        if (types == null) continue;

                        foreach (Type t in types)
                        {
                            try
                            {
                                if (t == null) continue;
                                String ns = t.Namespace;
                                if (ns == null) ns = String.Empty;

                                if (_typesForNamespace.ContainsKey(ns))
                                    list= (List<String>) _typesForNamespace[ns];
                                else
                                {
                                    list= new List<String>();
                                    _typesForNamespace[ns]= list;
                                }

                                // sometimes we get duplicate types
                                if (!list.Contains(t.FullName))
                                {
                                    list.Add(t.FullName);
                                    //_assemblyForType[t.FullName]= assemblyName;
                                    _assemblyForType[t.FullName]= shortName;
                                    var fixedName = FixupGenericTypeName(t.Name);
                                    if (_fullNamesForShortNames.ContainsKey(fixedName))
                                    {
                                        var x = _fullNamesForShortNames[fixedName];
                                        _fullNamesForShortNames[fixedName] =
                                            String.Format("{0}, {1}", t.FullName, x);
                                    }
                                    else
                                        _fullNamesForShortNames[fixedName] = t.FullName;
                                }
                            }
                            catch(ReflectionTypeLoadException)
                            {
                                //Response.Write("Problem with : " + t.FullName);
                                continue;
                            }
                        }
                    }
                    catch(Exception)
                    {
                        continue;
                    }
                }
            }

            return shortName; // assemblyName
        }


        private static void LoadAssembliesAndPopulateHashes()
        {
            _assembliesNotLoaded    = new Dictionary<String,Object>();
            _assemblyForType        = new Dictionary<String,String>();
            _typesForNamespace      = new Dictionary<String,List<String>>();
            _fullNamesForShortNames = new Dictionary<String,String>();
            _assembliesLoaded       = new Dictionary<String,Assembly>();

            foreach (var aname in StarterAssemblies)
            {
                InternalLoadOneAssembly(aname);
            }

            Alphabetize();
        }


        private static void Alphabetize()
        {
            foreach (string key in _typesForNamespace.Keys)
            {
                _typesForNamespace[key].Sort();
            }
        }


        public static String GetTypeInfo(String typeName)
        {
            string q= null;
            String[] s =  null;
            try
            {
                q= QualifyType(typeName);
                s = q.Replace(")","").Replace("(","").Split(" ".ToCharArray(), 3);
                return GetTypeInfo(s[1].Replace("\"",""), s[2].Replace("\"",""));

            }
            catch (System.Exception exc1)
            {
                System.Console.WriteLine("uncaught exception {0}", exc1.ToString());
                System.Console.WriteLine("q= {0}", q);
                System.Console.WriteLine("s.Length= {0}", s.Length);
                throw ;
            }
        }


        public static String GetTypeInfo(String typeName, String assemblyName)
        {
            try
            {
                if (_assemblyForType.Keys.Contains(typeName) &&
                    _assemblyForType[typeName] == assemblyName &&
                    _assembliesLoaded.Keys.Contains(assemblyName))
                {
                    Assembly a2 = _assembliesLoaded[assemblyName];
                    Ionic.Cscomp.TypeInfo ti2= new Ionic.Cscomp.TypeInfo(a2, typeName);
                    return ti2.AsSexp();
                }

                // Load from a strongname, eg
                // "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

                Assembly a= Assembly.Load(assemblyName);

                if ((a == null) && (System.IO.File.Exists(assemblyName)))
                    a= Assembly.LoadFrom(assemblyName);

                if (a == null)
                {
                    System.Console.Error.WriteLine("Cannot load that assembly");
                    return null;
                }

                Ionic.Cscomp.TypeInfo ti= new Ionic.Cscomp.TypeInfo(a, typeName);
                return ti.AsSexp();
            }
            catch(TypeLoadException e2)
            {
                Console.Error.WriteLine("TypeLoadException: Could not load type: \"{0}\"\n{1}", typeName, e2);

                return null;
            }
            catch (Exception e1)
            {
                Console.Error.WriteLine("Exception: type '{0}'\n{1}", typeName, e1);
                return null;
            }
        }



        private static Assembly TryLoadAssembly(String assemblyName)
        {
            Assembly a= null;

            if (assemblyName.Contains(','))
            {
                // smells like a fully-qualified name
                Tracing.Trace("TryLoadAssembly: loading as strong name");
                a= Assembly.Load(assemblyName);
            }
            else if (assemblyName.EndsWith(".dll") || assemblyName.EndsWith(".exe"))
            {
                Tracing.Trace("TryLoadAssembly: looks like a filename");
                if (System.IO.File.Exists(assemblyName))
                {
                    Tracing.Trace("TryLoadAssembly: file exists");
                    a= Assembly.LoadFrom(assemblyName) ;
                }
                else
                {
                    // look in search paths
                    var fullname = FindAssemblyInSearchPaths(assemblyName);
                    if (fullname != null && fullname != "nil")
                    {
                        a= Assembly.LoadFrom(fullname.Replace("\"", ""));
                    }
                }
            }
            else
            {
                var dll = GetShortDllName(assemblyName);
                var fullname = FindAssemblyInSearchPaths(dll);
                if (fullname != null && fullname != "nil")
                {
                    fullname = fullname.Replace("\"", "");
                    Tracing.Trace("TryLoadAssembly: LoadFrom({0})", fullname);
                    a= Assembly.LoadFrom(fullname);
                }
            }

            return a; // maybe null
        }



        private static System.Type TryLoadType(String theTypeName, String assemblyName)
        {
            System.Type t= null;
            Assembly a= TryLoadAssembly(assemblyName);
            if (a != null)
                t = a.GetType(theTypeName, false, true);
            return t;  // maybe null
        }


        public static String LoadOneAssembly (string name)
        {
            Tracing.Trace("LoadOneAssembly: {0}", name);
            try
            {
                string r = InternalLoadOneAssembly(name);

                if (r == null)
                {
                    Tracing.Trace("LoadOneAssembly: null");
                    return "nil";
                }

                Alphabetize();

                if (r == "t") {
                    Tracing.Trace("LoadOneAssembly: already loaded.");
                    return r;
                }

                var retval =  "\"" + r + "\"";
                // need this?  Don't think so.
                // Only if the return value is a path.
                retval = retval.Replace("\\", "\\\\");
                Tracing.Trace("returning: [{0}]", retval);
                return retval;
            }
            catch (System.Exception exc1)
            {
                Tracing.Trace("uncaught exception: {0} {1}", exc1, exc1.StackTrace);
                throw;
            }
        }



        public static String ListLoadedAssemblies ()
        {
            string atoms = String.Join("\" \"", _assembliesLoaded.Keys.ToArray());
            return "(list \"" + atoms + "\")";
        }


        /// <summary>
        ///   Gets the version of the assembly, in a string form.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Returns a quoted string, suitable for use as a lisp
        ///     s-expression.  Example: "1.2.0.4"
        ///   </para>
        /// </remarks>
        /// <returns>
        ///   The quoted version string
        /// </returns>
        public static String Version ()
        {
            return "\"" +
                System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString() + "\"";
        }


        public static String ListKnownTypes()
        {
            string atoms = String.Join("\" \"", _assemblyForType.Keys.ToArray());
            return "(list \"" + atoms + "\")" ;
        }



        /// <summary>
        ///   Gets all the known completions in the given namespace.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     The completions include all the types, and all the child namespaces.
        ///     So, for ns "System", the completion list will include System.Delegate
        ///     as well as System.Diagnostics
        ///   </para>
        /// </remarks>
        /// <returns>
        /// </returns>
        public static String GetCompletionsForNamespace(string ns)
        {
            if (String.IsNullOrEmpty(ns))
            {
                Tracing.Trace("GetCompletionsForNamespace: null input");
                return null;
            }

            if (!_typesForNamespace.ContainsKey(ns))
            {
                Tracing.Trace("GetCompletionsForNamespace: unknown ns '{0}'", ns);
                return null;
            }

            var result= new System.Text.StringBuilder();
            int len = ns.Length+1;
            result.Append("(list \"").Append(ns).Append("\" (list 'types (list ");
            foreach (var t in _typesForNamespace[ns])
            {
                var s = t.Substring(len);
                //System.Console.WriteLine("  " + t.Substring(len));
                //Tracing.Trace("    {0}", s);
                result.Append("\"").Append(s).Append("\" ");
            }
            result.Append("))");

            var childlist = new List<String>();
            foreach (var key in _typesForNamespace.Keys)
            {
                if (key.StartsWith(ns) && !key.Equals(ns))
                {
                    var child = key.Substring(len);
                    var p = child.IndexOf('.');
                    if (p > 0)
                        child = child.Substring(0,p);
                    if (!childlist.Contains(child))
                        childlist.Add(child);
                }
            }

            if (childlist.Count() > 0)
            {
                result.Append(" (list 'namespaces (list ");
                foreach (var c in childlist)
                    result.Append("\"").Append(c).Append("\" ");
                result.Append("))");
            }

            result.Append(")");
            return result.ToString();
        }


        private static string Escape(string s)
        {
            return s.Replace("\"", "\\\"");
        }


        /// <summary>
        ///   Qualify a name
        /// </summary>
        /// <returns>
        ///   ("type"       fulltypename)    if the name is a type
        ///   ("namespace"  parentNamespace) if the name is a namespace
        ///   ("unknown"    name)            if the name is a namespace
        /// </returns>
        public static String QualifyName(String name)
        {
            Tracing.Trace("QualifyName ({0})", name);

            var suffix = "." + name;
            IEnumerable<String> collection;

            //             if (!name.Contains("."))
            //             {
            //                 // no dot in the name = assume short name
            //                 collection = _fullNamesForShortNames.Keys;
            //
            //                 // check for exact match in the keys
            //                 foreach (var key in collection)
            //                 {
            //                     if (key.Equals(name))
            //                         return String.Format("(list \"type\" \"{0}\")",
            //                                              _fullNamesForShortNames[key]);
            //                 }
            //
            //                 return String.Format("(list \"unknown\" \"{0}\")",name);
            //             }

            if (Verbose)
                System.Console.WriteLine("checking name: {0}", name);

            // look for exact match on a fully-qualified typename
            collection = _fullNamesForShortNames.Values;
            foreach (var value in collection)
            {
                foreach (var v2 in value.Split(", ".ToCharArray()))
                {
                    if (v2.Equals(name))
                        return String.Format("(list \"type\" \"{0}\")", v2);
                }
            }

            // look for ends-with match on a fully-qualified typename
            foreach (var value in collection)
            {
                foreach (var v2 in value.Split(", ".ToCharArray()))
                {
                    if (v2.EndsWith(suffix))
                        return String.Format("(list \"type\" \"{0}\")", v2);
                }
            }

            // now check for exact match on known namespaces...
            collection = _typesForNamespace.Keys;
            foreach (var key in collection)
            {
                if (key.Equals(name))
                    return String.Format("(list \"namespace\" \"{0}\")", name);
            }

            // Match on the last segment of the namespace.  Eg, if name is "Diagnostics",
            // should match on "System.Diagnostics".
            foreach (var key in collection)
            {
                if (key.EndsWith(suffix))
                    return String.Format("(list \"namespace\" \"{0}\")", key);
            }

            Tracing.Trace("checking GAC...");
            // Finally, check the names of assemblies in the gac.
            // If found, load the assembly.
            collection = _GacAssemblies;
            foreach (var strongname in collection)
            {
                var parts = strongname.Split(", ".ToCharArray());
                if (parts!= null && parts[0].Equals(name))
                {
                    var r = LoadOneAssembly(strongname);
                    if (r!=null && r != "nil")
                        return String.Format("(list \"namespace\" \"{0}\")", name);
                }
            }

            return String.Format("(list \"unknown\" \"{0}\")", Escape(name));
        }



        /// <summary>
        ///   Return all possible matches on a given symbol fragment.
        /// </summary>
        ///
        /// <param name='fragment'>
        ///   the fragment of the name to match on.
        /// </param>
        ///
        /// <param name='namespaces'>
        ///   a comma-separated list of namespaces
        /// </param>
        ///
        /// <returns>
        ///   a list containing pairs of all possible completions.
        ///   eg, if completing on Ba?, maybe return:
        ///   (list
        ///      ("type"      "Foo.Bar")
        ///      ("type"      "Utils.Bands")
        ///      ("namespace"  "Barrels")
        ///   )
        /// </returns>
        public static String GetMatches(String fragment, String namespaces)
        {
            if (String.IsNullOrEmpty(fragment))
                return "nil";

            List<String> responseSet = new List<String>();
            var reTypeStub = "\\." + fragment + ".*$";
            var reNamespace = "^" + fragment + ".*$";
            IEnumerable<String> collection;
            Tracing.Trace("checking fragment: {0}", fragment);

            // look for types with short names that begin with the fragment
            collection = _fullNamesForShortNames.Values;
            foreach (string ns in namespaces.Split(','))
            {
                foreach (var value in collection)
                {
                    foreach (var v2 in value.Split(", ".ToCharArray()))
                    {
                        Match match = Regex.Match(v2,"^"+ns+reTypeStub);
                        if (match.Success)
                            responseSet.Add(String.Format("(list \"type\" \"{0}\")", v2));
                    }
                }
            }

            // look for namespaces that begin with the fragment
            collection = _typesForNamespace.Keys;
            foreach (var key in collection)
            {
                Match match = Regex.Match(key,reNamespace);
                if (match.Success)
                    responseSet.Add(String.Format("(list \"namespace\" \"{0}\")", key));

                // I think maybe we want to exclude child namespaces. . .
                // maybe later.
            }

            if (responseSet.Count == 0)
                return "nil";

            string items = String.Join(" ", responseSet.ToArray());
            return "(list " + items + ")";
        }


        public static string FixupGenericTypeName(string typeName)
        {
            var name = typeName;
            Match match = Regex.Match(name,"(.+)`([1-9])$");
            if (match.Success)
                name = match.Groups[1].Value.ToString();
            return name;
        }


        public static String QualifyType(String typeName)
        {
            return QualifyType(typeName, null);
        }


        /// <summary>
        ///   Qualifies the type name.
        /// </summary>
        /// <param name='typeName'>
        ///   the name of the type, possibly a short name, like "Console" or "Stream",
        ///   and possibly a long name like System.IO.Stream
        /// </param>
        /// <param name='usinglist'>
        ///   a list of namespaces referenced at the top of the module in using
        ///   statements. Favor these namespaces when doing type qualification.
        /// </param>
        /// <returns>
        ///   sexp:  (list "fulltypename" "assemblyname") or nil if the type is not known
        ///   The assembly name
        /// </returns>
        public static String QualifyType(String typeName, String usinglist)
        {
            string stub = null;
            System.Text.StringBuilder residual = null;
            int repeats = 0;

            Tracing.Trace("QualifyType: {0}", typeName);

            // fixup generic type
            var name = typeName;
            Match match = Regex.Match(name,"(.+`[1-9])\\[.+\\]$");
            if (match.Success)
                name = match.Groups[1].Value.ToString();

            name = name.Trim();
            Tracing.Trace("QualifyType: name '{0}'", name);

            if (!name.Contains("."))
            {
                Tracing.Trace("QualifyType: name contains no dot");
                Tracing.Trace("QualifyType: examining {0} short names",
                              _fullNamesForShortNames.Keys.Count);

                foreach (var key in _fullNamesForShortNames.Keys)
                {
                    var value = _fullNamesForShortNames[key];

                    if (key.Equals(name))
                    {
                        string tname = null;
                        if (_fullNamesForShortNames[key].Contains(','))
                        {
                            var nlist = _fullNamesForShortNames[key].Split(", ".ToCharArray());
                            if (usinglist != null)
                            {
                                var ulist = usinglist.Split(", ".ToCharArray());

                                var GetNamespace = new Func<string,string>((s) =>
                                    {
                                        int ix = s.LastIndexOf('.') ;
                                        if (ix <= 0)
                                            return null;
                                        return s.Substring(0,ix);
                                    });

                                var selection = from fn in nlist
                                    join u in ulist on GetNamespace(fn) equals u
                                    select fn;

                                int c = selection.Count();

                                tname = (c>=1)
                                    ? selection.First()
                                    : nlist[0];
                            }
                            else
                                tname = nlist[0];
                        }
                        else
                        {
                            tname = _fullNamesForShortNames[key];
                        }

                        return
                            String.Format("(list \"{0}\" \"{1}\")",
                                          tname,
                                          _assemblyForType[tname]);
                    }
                }
            }

            // it may be a fully- or partially qualified type name
            Tracing.Trace("QualifyType: examining {0} full names",
                          _fullNamesForShortNames.Values.Count);

            foreach (var v1 in _fullNamesForShortNames.Values)
            {
                foreach (var value in v1.Split(", ".ToCharArray()))
                {
                    if (stub == null || !value.StartsWith(stub))
                    {
                        int ix = value.LastIndexOf('.');
                        stub= (ix > 0)
                            ? value.Substring(0, ix)
                            : value ;
                        repeats = 0;
                        if (residual!=null)
                        {
                            var r = residual.ToString();
                            if (!String.IsNullOrEmpty(r))
                                Tracing.Trace("  {0}", r);
                        }

                        residual = new System.Text.StringBuilder();
                    }
                    else
                    {
                        residual.Append(".");
                        repeats++;
                    }

                    // if (repeats == 0)
                    //     Tracing.Trace("  check: {0}.*", stub);

                    // check for exact match
                    if (value.Equals(name))
                    {
                        return
                            String.Format("(list \"{0}\" \"{1}\")",
                                          value,
                                          _assemblyForType[value]);
                    }
                }
            }

            return "nil";
        }




        public static string GetConstructors (string typeName)
        {
            Tracing.Trace("GetConstructors: {0}", typeName);
            if (!_assemblyForType.Keys.Contains(typeName))
            {
                Tracing.Trace("GetConstructors: unknown assembly, found 0 constructors");
                return "nil";
            }

            Assembly a = AssemblyIsLoaded(_assemblyForType[typeName]);
            if (a==null)
            {
                Tracing.Trace("GetConstructors: could not load assembly, found 0 constructors");

                return "nil";
            }

            var tinfo= new Ionic.Cscomp.TypeInfo(a, typeName);
            return tinfo.GetConstructorsSexp();
        }



        public static string GetTypeGivenVarDecl (string csharpVarDeclaration)
        {
            return GetTypeGivenVarDecl(csharpVarDeclaration,
                                       null,
                                       null,
                                       -1,
                                       "Foo1", // classname
                                       "", "");
        }


        /// <summary>
        ///   Get the type of the Nth variable in the declaration list.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     This method compiles the provided variable declaration list
        ///     into an assembly, and then performs reflection on the result.
        ///   </para>
        /// </remarks>
        ///
        /// <param name='csharpVarDeclaration'>
        ///   a fragment of csharp code to compile.  It should contain one or
        ///   more variable declarations.
        /// </param>
        /// <param name='varIndex'>
        ///   the number of the variable to determine the type of.
        ///   There's no way to interrogate the type of a local var by
        ///   name. That information is not in the assembly metadata -
        ///   it's in the PDB, and it's not easily accessible.  But, the
        ///   MSIL apparently lists the local variables in the order in
        ///   which they were declared. So we can use the index to find
        ///   the desired local var.
        /// </param>
        /// <param name='namespaces'>
        ///   a comma-separated list of namespaces
        /// </param>
        /// <param name='references'>
        ///   a comma-separated list of assembly references
        /// </param>
        /// <param name='classname'>
        ///   name of the class to use. Helpful in satisfying static references.
        /// </param>
        /// <param name='arglist'>
        ///   a comma-separated list of method types and arguments. This is
        ///   a legal fragment of C# code, suitable to put right into the method
        ///  signature.
        /// </param>
        public static string GetTypeGivenVarDecl (string csharpVarDeclaration,
                                                  String namespaces,
                                                  String assemblyReferences,
                                                  int varIndex,
                                                  String classname,
                                                  String arglist,
                                                  String instanceMembers)
        {
            try
            {
                Tracing.Trace("GetTypeGivenVarDecl {0}", csharpVarDeclaration);
                foreach (string aName in namespaces.Split(','))
                {
                    Tracing.Trace("Autoload assy {0}", aName);
                    InternalLoadOneAssembly(aName);
                }

                Object compileResult = CompileFragment(csharpVarDeclaration,
                                                       namespaces,
                                                       assemblyReferences,
                                                       classname,
                                                       arglist,
                                                       instanceMembers);

                MethodInfo methodInfo = compileResult as MethodInfo;
                if (methodInfo == null)
                {
                    String[] emsgs = (String[]) compileResult;
                    var estring = (emsgs!=null)
                        ? String.Join("\" \"", emsgs)
                        : String.Format("unknown error (cr = {0})",
                                        (compileResult==null)?"-null-":compileResult.ToString());
                    return String.Format("(list \"error\" \"{0}\")", estring.Replace("\\", "\\\\"));
                }


                var res = GetTypeOfNthLocalVar(methodInfo, varIndex).ToString();
                res = String.Format("(list \"type\" \"{0}\")", res);

                Tracing.Trace("GetTypeGivenVarDecl: result {0}", res);

                return res;
            }
            catch (System.Exception exc1)
            {
                Tracing.Trace("GetTypeGivenVarDecl Exception: {0}", exc1.ToString());
                return String.Format ("(list \"exception\" \"{0}\")", exc1.Message);
            }
        }


        public static String SetAssemblySearchPaths(String commaDelimitedListOfPaths)
        {
            if (!String.IsNullOrEmpty(commaDelimitedListOfPaths))
            {
                SetBasicSearchPaths();
                _SearchPaths.AddRange(commaDelimitedListOfPaths
                                      .Split(",".ToCharArray(),
                                             StringSplitOptions.RemoveEmptyEntries));
            }

            string atoms = String.Join("\" \"", _SearchPaths.ToArray());
            return "(list \"" + atoms.Replace("\\", "\\\\") + "\")";
        }


        /// <summary>
        ///   Add one path to the list of assembly search paths
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     If path is an empty string, no change will happen, but
        ///     the caller will get the list of search paths in return.
        ///   </para>
        /// </remarks>
        /// <returns>
        ///   A lisp s-expression containing the list of search paths.
        /// </returns>
        public static String AddAssemblySearchPath(String path)
        {
            if (!String.IsNullOrEmpty(path))
                _SearchPaths.Add(path);

            string atoms = String.Join("\" \"", _SearchPaths.ToArray());
            return "(list \"" + atoms + "\")";
        }


        public static String GetAstForSourceFile(String filename)
        {
            Tracing.Trace("GetAstForSourceFile: {0}", filename);
            try
            {
                return AstUtils.CreateAstSexpression(filename);
            }
            catch (System.IO.FileNotFoundException)
            {
                return "(list 'error 'System.IO.FileNotFoundException)";
            }
            catch (System.Exception exc1)
            {
                Tracing.Trace("uncaught exception: {0}", exc1);
                System.Console.WriteLine("uncaught exception: {0}", exc1);
            }
            return "nil";
        }



        public static string FindAssemblyInSearchPaths(string filename)
        {
            if (File.Exists(filename)) return filename;

            var selection = from p in _SearchPaths
                where File.Exists(Path.Combine(p,filename))
                select Path.Combine(p,filename);

            if (selection.Count() >= 1)
            {
                Tracing.Trace("found assembly:{0}=>{1}", filename,
                              selection.First());
                return "\"" + selection.First() + "\"";
            }

            Tracing.Trace("could not find assembly:{0}", filename);
            return "nil"; // cannot find
        }



        private static string GetShortAssemblyName(string ns)
        {
            switch (ns)
            {
                case "System.Text":
                    return "System";

                case "System.Linq":
                    return "System.Core";

                case "System.Web.Mvc.Ajax":
                    return "System.Web.Mvc";

                case "System.Xml.Serialization":
                    return "System.Xml";

                case "ICSharpCode.NRefactory.Ast":
                    return "ICSharpCode.NRefactory";

                default:
                    return ns;
            }
        }




        private static string GetShortDllName(string ns)
        {
            return GetShortAssemblyName(ns) + ".dll";
        }



        public static string GetAssemblyPathForNamespace(String ns)
        {
            Tracing.Trace("GetAssemblyPathForNamespace {0}", ns);
            var aname = GetShortAssemblyName(ns);
            Assembly a = AssemblyIsLoaded(aname);
            if (a!=null)
            {
                Tracing.Trace("  isloaded, location '{0}'", a.Location);
                return "\"" + a.Location.Replace("\\", "\\\\") + "\"";
            }

            Tracing.Trace("  looking in search paths");
            var result = FindAssemblyInSearchPaths(GetShortDllName(aname));
            if (result != null && result != "nil")
                return "\"" + result.Replace("\\", "\\\\") + "\"";
            return result;
        }



        private static string GetUsingClauses(IEnumerable<String> namespaces)
        {
            string s = "";
            foreach (var ns in namespaces)
            {
                if (!String.IsNullOrEmpty(ns))
                    s += "using " + ns + ";\n";
            }
            return s;
        }



        /// <summary>
        ///   Compile a C# var declaration fragment.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     The goal is to determine the types of the local variables,
        ///     so that we can do completion on them.
        ///   </para>
        /// </remarks>
        /// <param name='fragment'>
        ///   a comma-separated list of namespaces
        /// </param>
        /// <param name='namespaces'>
        ///   a comma-separated list of namespaces
        /// </param>
        /// <param name='references'>
        ///   a comma-separated list of assembly references
        /// </param>
        /// <param name='classname'>
        ///   name of the class to use. Helpful in satisfying static references.
        /// </param>
        /// <param name='arglist'>
        ///   a C# fragment, representing an arglist, suitable for
        ///   framing within parens for the method declaration.
        /// </param>
        /// <returns>
        ///   a MethodInfo, if success.  Otherwise, an array of Strings containing
        ///   error messages.
        /// </returns>
        private static Object CompileFragment(string fragment,
                                              String namespaces,
                                              String references,
                                              string classname,
                                              String arglist,
                                              String instanceMembers)
        {
            string nsname = "N"+Path.GetRandomFileName().Replace(".","");
            string methodname = "M"+Path.GetRandomFileName().Replace(".","");
            //string classname = Path.GetRandomFileName().Replace(".","");
            string usingBlock = (String.IsNullOrEmpty(namespaces))
                ? "using System;\n"
                : GetUsingClauses(namespaces.Split(','));
            string literalSource =
                usingBlock +
                "\nnamespace " + nsname + " {" +
                "\n  class " + classname + " {" +
                "\n    " + instanceMembers.Replace("; ",";\n    ")
                .Replace("} ","}\n    ")
                .Replace(";}","; }") +
                "\n    void "+ methodname + " (" + arglist + ") {"+
                "\n       " + fragment.Replace(";",";\n      ") +
                "\n    " +
                "}\n  }\n}\n";

            if (Verbose)
                System.Console.WriteLine("code to compile:\n{0}", literalSource);

            Tracing.Trace("code to compile:\n{0}", literalSource);

            try
            {
                var csharp = new Microsoft.CSharp.CSharpCodeProvider(new Dictionary<String, String> { { "CompilerVersion", "v3.5" } });

                var cp = new System.CodeDom.Compiler.CompilerParameters();
                cp.GenerateInMemory = true;
                cp.GenerateExecutable = false;
                cp.IncludeDebugInformation = true;

                if (!String.IsNullOrEmpty(references))
                    foreach (string ra in references.Split(','))
                        cp.ReferencedAssemblies.Add(ra);

                // do I need to worry about duplicates?
                foreach (string aName in namespaces.Split(','))
                {
                    Tracing.Trace("maybe add Ref: {0}", aName);
                    var path = GetAssemblyPathForNamespace(aName);
                    if (path!=null && path != "nil")
                        cp.ReferencedAssemblies.Add(path);
                }

                System.CodeDom.Compiler.CompilerResults cr =
                    csharp.CompileAssemblyFromSource(cp,literalSource);
                if (cr == null)
                {
                    var e = new List<String>();
                    Tracing.Trace("CompilerResults == null");
                    e.Add("CompilerResults == null");
                    return e.ToArray();
                }

                foreach (string s in cr.Output)
                {
                    if (Verbose)
                        System.Console.WriteLine(s);
                    Tracing.Trace(s);
                }

                if (cr.Errors.Count != 0)
                {
                    var e = new List<String>();
                    e.Add(String.Format("Errors.Count = {0}", cr.Errors.Count));
                    e.Add(cr.Errors[0].ToString());
                    foreach(var error in cr.Errors)
                        Tracing.Trace(error.ToString());
                    return e.ToArray();
                }

                var a = cr.CompiledAssembly;
                MethodInfo mi = a.GetType(nsname + "." + classname)
                    .GetMethod(methodname, BindingFlags.Instance | BindingFlags.NonPublic);

                return mi;
            }
            catch (System.Exception e1)
            {
                var e = new List<String>();
                e.Add("Exception during compile: " + e1.ToString() );
                Tracing.Trace("{0}", e1.ToString());
                return e.ToArray();
            }
        }



        /// <summary>
        ///   Get the type of the Nth local variable in the given method.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Reflection doesn't provide access to var names.  And,
        ///     some variables listed in LocalVariables are synthetic -
        ///     generated by the compiler.  But apparently the IL does
        ///     list the format local vars first, and in the order in
        ///     which they are declared.  So, to retrieve a local var,
        ///     rely on the count.
        ///   </para>
        /// </remarks>
        private static Type GetTypeOfNthLocalVar(MethodInfo methodInfo, int ix)
        {
            MethodBody body = methodInfo.GetMethodBody();
            var localVars = body.LocalVariables;
            Type ft = null;
            int c = 0;
            foreach (LocalVariableInfo lvi in localVars)
            {
                if (Verbose)
                    System.Console.WriteLine("Local var [{0}] {1}",
                                             c, lvi.LocalType.ToString());
                if (c==ix || ix < 0)
                    ft = lvi.LocalType;
                c++;
            }
            return ft;
        }

        private static bool _Verbose;
        public static bool Verbose
        {
            get
            {
                return _Verbose;
            }
            set
            {
                if (value)
                {
                    // turn on
                    Tracing.Trace("Verbose = true");
                }
                _Verbose = value;
            }
        }
    }



    public class TypeInfo
    {
        internal TypeInfo(Assembly a, string typeName)
        {
            mt = a.GetType(typeName, false, true);
            if ( mt == null )
                throw new Exception(String.Format("Cannot get that type ({0})", typeName));
        }

        /// <summary>
        ///   Returns the TypeInfo as a lisp S-expression.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     It looks like this:
        ///
        ///   </para>
        /// </remarks>
        internal String AsSexp()
        {
            var result= new System.Text.StringBuilder();
            result.Append("(list \"").Append(mt.FullName).Append("\" 'type (list ");

            // properties
            var props = GetPropertiesInfo();
            if (props != null)
            {
                foreach (string s in props)
                    result.Append(s);
            }

            result.Append(") (list ");

            // methods
            var meth = GetMethodsInfo();
            if (meth != null)
            {
                foreach (string s in meth)
                    result.Append(s);

            }
            result.Append(") (list ");


            // fields
            var fields = GetFieldsInfo();
            if (fields != null)
            {
                foreach (string s in fields)
                    result.Append(s);

            }
            result.Append(")");

            // events?  constructors?

            result.Append(")");
            return result.ToString();
        }

        //     public String LastError { get { return lastError;}  }
        //     public Type   TheType   { get { return mt;      }  }
        //     public String FQN       { get { return (mt!=null) ? mt.AssemblyQualifiedName: null; }  }
        //     public String Name      { get { return (mt!=null) ? mt.Name: null;}  }
        //     public String FullName  { get { return (mt != null) ? mt.FullName : null;}  }
        //     public String Status    { get { return status;}  }

        //     public String Href
        //     {
        //       set { theHref= value; }
        //       get { return theHref; }
        //     }

        private static Type ReferencedType(Type t)
        {
            return (t.HasElementType) ? ReferencedType(t.GetElementType()) : t ;
        }

        private static String EmitOneType(Type t1)
        {
            return t1.ToString();
        }


        private static String EmitOneTypeWithInterfaces(Type t1)
        {
            Type t= ReferencedType(t1);
            var result= new System.Text.StringBuilder();
            result.Append(t1.ToString());

            Type[] it=  t.GetInterfaces();
            if (it.Length > 0 )
            {
                int j=0;
                for (int i=0; i < it.Length; i++)
                {
                    if ((t.BaseType!=null) && !(it[i].IsAssignableFrom(t.BaseType)))
                    {
                        if (j==0) result.Append(" : ");
                        else result.Append(", ");
                        result.Append(EmitOneType(it[i]));
                        j++;
                    }
                }
            }

            return result.ToString();
        }



        private string ParamString(System.Reflection.ParameterInfo[] pi, bool isProperty)
        {
            if (pi.Length==0) return "nil";

            var sb=  new System.Text.StringBuilder("(list ");
            int j;
            String openBracket= "(";
            String closeBracket= ")";
            if (isProperty)
            {
                openBracket= "[";
                closeBracket= "]";
            }

            for (j=0; j < pi.Length; j++)
            {
                // ParameterAttributes Attributes =  pi[j].Attributes;
                // if (Attributes != ParameterAttributes.None)
                //     sb.Append("[").Append(Attributes.ToString()).Append("] "); // eg, In,Out,Optional

                sb.Append("\"").Append(pi[j].ParameterType.ToString())
                    .Append(" ")
                    .Append(pi[j].Name)
                    .Append("\" ");
            }
            if (isProperty)
            {
                if (j!=0) sb.Append(closeBracket);
            }
            else
            {
                if (j==0) sb.Append(openBracket);
                sb.Append(closeBracket);
            }

            return sb.ToString();
        }


        private string NewParamString(System.Reflection.ParameterInfo[] pi)
        {
            if (pi.Length==0) return "nil";
            var sb=  new System.Text.StringBuilder("(list ");
            for (int j=0; j < pi.Length; j++)
            {
                sb.Append("(list \"")
                    .Append(pi[j].Name)
                    .Append("\" 'variable (list :type \"")
                    .Append(pi[j].ParameterType.ToString())
                    .Append("\")) ");
            }
            sb.Append(")");
            return sb.ToString();
        }


        private static String EmitAttributes(Type t) {
            System.Text.StringBuilder result= new System.Text.StringBuilder();
            if (t.IsSerializable)   result.Append("[serializable]<br>\n ");
            if (t.IsPublic)         result.Append("public ");
            if (t.IsAbstract)       result.Append("abstract ");
            if (t.IsSealed)         result.Append("sealed ");
            if (t.IsClass)          result.Append("class ");
            if (t.IsEnum)           result.Append("enum ");
            if (t.IsInterface)      result.Append("interface ");
            return result.ToString();
        }


        internal String GetConstructorsSexp()
        {
            if (mt==null)
            {
                Tracing.Trace("{0}: no type?", mt.FullName);
                return "nil";
            }
            System.Reflection.ConstructorInfo[] cia= mt.GetConstructors();
            if (cia==null) return "nil";
            if (cia.Length==0)
            {
                Tracing.Trace("{0}: No constructors for that type", mt.FullName);
                return "nil";
            }

            Tracing.Trace("{1}: Found {0} constructors", cia.Length, mt.FullName);

            var sb1 = new System.Text.StringBuilder();
            sb1.Append("(list \"").Append(mt.FullName)
                .Append("\" 'type (list :constructors (list\n");

            foreach (ConstructorInfo ci in cia)
            {
                sb1.Append(" (list :typemodifiers ")
                    .Append(NewMethodBaseModifiers(ci))
                    .Append(" :arguments ")
                    .Append(NewParamString(ci.GetParameters()))
                    .Append(")\n");
            }
            sb1.Append(")))");
            return sb1.ToString();
        }



        private String PropertyModifiers(PropertyInfo p)
        {
            var sb= new System.Text.StringBuilder("(cons 'typemodifiers (list ");
            System.Reflection.MethodInfo mi= null;

            if (p.GetGetMethod() != null)
            {
                mi= p.GetGetMethod();
                if (p.GetSetMethod() == null)
                    sb.Append("\"readonly\" ");
            }
            else if (p.GetSetMethod() != null) {
                mi= p.GetSetMethod();
                sb.Append("\"writeonly\" ");
            }

            if (mi != null) {
                if (mi.IsPublic)
                    sb.Append("\"public\" ");
                if (mi.IsPrivate)
                    sb.Append("\"private\" ");
                if (mi.IsFamily)
                    sb.Append("\"protected\" ");

                if (mi.IsStatic)
                    sb.Append("\"static\" ");
            }

            sb.Append("))");
            return sb.ToString();
        }


        private String[] GetPropertiesInfo()
        {
            if (mt==null) return null;
            System.Reflection.PropertyInfo[] pi= mt.GetProperties();
            if (pi==null) return null;
            if (pi.Length==0) return null;

            var a= new List<String>();
            System.Text.StringBuilder sb1;
            foreach (PropertyInfo p in pi) {

                sb1 = new System.Text.StringBuilder();
                sb1.Append("(list \"").Append(p.Name).Append("\" 'property ");

                sb1.Append("\"").Append(p.PropertyType.ToString()).Append("\" ");

                sb1.Append(PropertyModifiers(p));

                sb1.Append(")");

                a.Add(sb1.ToString());
            }
            return a.ToArray();
        }



        private String EmitMethodAttrs(MethodInfo m) {
            System.Text.StringBuilder result= new System.Text.StringBuilder();

            if (m.IsPublic)
                result.Append("public ");

            if (m.IsFamily)
                result.Append("protected ");

            if (m.IsPrivate)
                result.Append("private ");

            if (m.IsAbstract)
                result.Append("abstract ");

            if (m.IsStatic)
                result.Append("static ");

            if (m.IsFinal)
                result.Append("final ");

            return result.ToString();
        }



        public String[] GetMethodsInfo()
        {
            System.Reflection.MethodInfo[] mi= mt.GetMethods();
            System.Array.Sort(mi,new MpfComparer());
            var a= new List<String>();
            System.Text.StringBuilder sb1;
            foreach (MethodInfo m in mi)
            {
                sb1= null;

                if (m.IsPrivate) continue;

                // special name denotes???? I don't know.
                if (!m.IsSpecialName)
                {
                    // it's a method:
                    sb1 = new System.Text.StringBuilder(" (list \"");
                    sb1.Append(m.Name).Append("\" 'method ")
                        .Append("\"").Append(m.ReturnType.ToString()).Append("\" ")
                        .Append(ParamString(m.GetParameters(), false))
                        .Append(MethodBaseModifiers(m))
                        .Append(")");
                }

                if (sb1 != null)
                    a.Add(sb1.ToString());
            }
            return a.ToArray();
        }



        private string MethodBaseModifiers(MethodBase mi)
        {
            var sb= new System.Text.StringBuilder(" (cons 'typemodifiers (list ");
            if (mi.IsFinal)
                sb.Append("\"sealed\" ");
            if (mi.IsPublic)
                sb.Append("\"public\" ");
            if (mi.IsPrivate)
                sb.Append("\"private\" ");
            if (mi.IsFamily)
                sb.Append("\"protected\" ");
            if (mi.IsStatic)
                sb.Append("\"static\" ");
            sb.Append("))");
            return sb.ToString();
        }


        private string NewMethodBaseModifiers(MethodBase mi)
        {
            var sb= new System.Text.StringBuilder(" (list ");
            if (mi.IsFinal)
                sb.Append("\"sealed\" ");
            if (mi.IsPublic)
                sb.Append("\"public\" ");
            if (mi.IsPrivate)
                sb.Append("\"private\" ");
            if (mi.IsFamily)
                sb.Append("\"protected\" ");
            if (mi.IsStatic)
                sb.Append("\"static\" ");
            sb.Append(")");
            return sb.ToString();
        }


        //     private void BuildClassHierarchy(Type t) {
        //       classHierarchy.Push(t);
        //       if (t.BaseType != null)
        //      BuildClassHierarchy(t.BaseType);
        //     }


        //     public String GetHierarchyHtml() {
        //       if (mt == null) return null;
        //       classHierarchy= new Stack();
        //       BuildClassHierarchy(mt);
        //       int c= classHierarchy.Count;
        //       System.Text.StringBuilder sb= new System.Text.StringBuilder("\n");
        //       int i=0;
        //       while (classHierarchy.Count != 0) {
        //      sb.Append("<div class='elt'>");
        //      if (i!=0) sb.Append("+&nbsp;");
        //      sb.Append(EmitOneTypeWithInterfaces((Type)classHierarchy.Pop())).Append("\n");
        //      i++;
        //       }
        //       for (i=0; i < c; i++) sb.Append("</div>");

        //       return sb.ToString();
        //     }




        //     public String[] GetPropertiesHtml() {
        //       if (mt == null) return null;
        //       System.Reflection.PropertyInfo[] pi= mt.GetProperties();
        //       System.Array.Sort(pi,new myComparer());  // by name
        //       ArrayList a= new ArrayList();
        //       System.Text.StringBuilder sb1;
        //       foreach (PropertyInfo p in pi) {
        //      try{
        //        sb1 = new System.Text.StringBuilder();
        //        sb1.Append("  ").Append(PropAttrsString(p));
        //        sb1.Append("  ").Append(EmitOneType(p.PropertyType));
        //        sb1.Append("  <b>").Append(p.Name).Append("</b>");
        //        AppendParams(sb1,p.GetIndexParameters(), true);
        //      }
        //      catch(Exception e){
        //        a.Add(e.Message);
        //        continue;
        //      }
        //      a.Add(sb1.ToString());
        //       }
        //       return (String[]) a.ToArray(typeof(String));
        //     }



        private static String FieldAttrsString(FieldInfo f)
        {
            var sb= new System.Text.StringBuilder(" (cons 'typemodifiers (list ");
            if (f.IsPublic)
                sb.Append("\"public\" ");
            if (f.IsFamily)
                sb.Append("\"protected\" ");
            if (f.IsPrivate)
                sb.Append("\"private\" ");
            if (f.IsLiteral)
                sb.Append("\"const\" ");
            if (f.IsStatic)
                sb.Append("\"static\" ");
            sb.Append("))");
            return sb.ToString();
        }


        public String[] GetFieldsInfo()
        {
            if (mt == null) return null;
            System.Reflection.FieldInfo[] fi= mt.GetFields();
            System.Array.Sort(fi,new MpfComparer());  // by name
            var a= new List<String>();
            System.Text.StringBuilder sb1;
            foreach (FieldInfo f in fi)
            {
                if (!f.IsPrivate)
                {
                    try
                    {
                        sb1 = new System.Text.StringBuilder(" (list \"");
                        sb1.Append(f.Name).Append("\" 'field ")
                            .Append("\"").Append(f.FieldType.ToString()).Append("\" ")
                            .Append(FieldAttrsString(f))
                            .Append(")");
                    }
                    catch(Exception e){
                        a.Add(e.Message);
                        continue;
                    }
                    if (sb1 != null)
                        a.Add(sb1.ToString());
                }
            }
            return a.ToArray();
        }

        private System.Type mt;
    }



    public class MpfComparer : System.Collections.IComparer
    {
        static Type mi= typeof(MethodInfo);
        static Type pi= typeof(PropertyInfo);
        static Type fi= typeof(FieldInfo);

        public int Compare (Object x, Object y)
        {
            if (mi.IsInstanceOfType(x)) {
                MethodInfo i1= (MethodInfo) x;
                MethodInfo i2= (MethodInfo) y;
                return i1.Name.CompareTo(i2.Name);
            }

            if (pi.IsInstanceOfType(x)) {
                PropertyInfo i1= (PropertyInfo) x;
                PropertyInfo i2= (PropertyInfo) y;
                return i1.Name.CompareTo(i2.Name);
            }
            if (fi.IsInstanceOfType(x)) {
                FieldInfo i1= (FieldInfo) x;
                FieldInfo i2= (FieldInfo) y;
                return i1.Name.CompareTo(i2.Name);
            }

            return 0;
        }
    }


    internal static class Tracing
    {
#if UseCopyData
        private static  Ionic.CopyData.Transceiver transceiver;
        private static bool _initialized;
#endif

        [Conditional("CscompTrace")]
        private static void SetupDebugConsole()
        {
#if UseCopyData

            // use object initializer syntax
            System.Diagnostics.Process p3 = new System.Diagnostics.Process
                {
                    StartInfo =
                    {
                        FileName = "c:\\dev\\vsp\\UnitTestProgressMonitor\\UnitTestProgressMonitor\\bin\\Debug\\UnitTestProgressMonitor.exe",
                        Arguments = "-channel CscompShell",
                        CreateNoWindow = false,
                        UseShellExecute = false
                    }
                };
            p3.Start();

            // wait for the process to start?
            System.Threading.Thread.Sleep(650);

            transceiver = new Ionic.CopyData.Transceiver();
            transceiver.Channel = "CscompShell";

            transceiver.Send("title *CscompShell* Trace Monitor");

            System.Threading.Thread.Sleep(400);
            transceiver.Send("log Hello from CscompShell");

            _initialized = true;
#endif
        }


        [Conditional("CscompTrace")]
        public static void Trace(string format, params object[] args)
        {
#if UseCopyData
            if (!_initialized)
            {
                SetupDebugConsole();
            }

            var s2 = String.Format(format, args);
            transceiver.Send("log " + s2);
#endif
        }
    }


    static class AstUtils
    {

        internal static string CreateAstSexpression(string filename)
        {
            using (var fs = File.OpenRead(filename))
            {
                using (var parser = ParserFactory.CreateParser(SupportedLanguage.CSharp,
                                                               new StreamReader(fs)))
                {
                    parser.Parse();

                    // RetrieveSpecials() returns an IList<ISpecial>
                    // parser.Lexer.SpecialTracker.RetrieveSpecials()...
                    // "specials" == comments, preprocessor directives, etc.

                    // parser.CompilationUnit retrieves the root node of the result AST
                    return SexpressionGenerator.Generate(parser.CompilationUnit).ToString();

                    // if (parser.Errors.Count > 0)
                    // {
                    //     MessageBox.Show(parser.Errors.ErrorOutput, "Parse errors");
                    // }
                }
            }
        }


        sealed class Nonce
        {
            private readonly static Nonce _instance = new Nonce();
            public static Nonce Instance  { get { return _instance; } }

            int value;
            public int Value
            {
                get
                {
                    return value++;
                }
            }
            private Nonce()
            {
                value = 1;
            }
        }


        class SexpressionGenerator
        {
            private int depth = 0;
            private StringBuilder sb1;

            static internal StringBuilder Generate(ICSharpCode.NRefactory.Ast.AbstractNode node)
            {
                var sexp = new SexpressionGenerator();
                return sexp.NodeToStringBuilder(node, null);
            }


            private void EmitModifier (AttributedNode anode)
            {
                sb1.Append('\n')
                    .Append(new String(' ', 2*depth + 2))
                    .Append("(modifier \"");
                if (anode.Modifier.ToString() != "None")
                    sb1.Append(anode.Modifier.ToString().ToLower().Replace(",",""));
                sb1.Append("\") ");
            }

            private void EmitParameters(ParametrizedNode pnode)
            {
                string indent = new String(' ', 2*depth + 2);
                sb1.Append('\n')
                    .Append(indent)
                    .Append("(params");
                if (pnode.Parameters == null || pnode.Parameters.Count == 0)
                {
                    sb1.Append(" nil)");
                }
                else
                {
                    foreach (var p in pnode.Parameters)
                    {
                        sb1.Append('\n')
                            .Append(indent)
                            .Append("  (var \"")
                            .Append(p.ParameterName)
                            .Append("\" \"")
                            .Append(p.TypeReference.ToString())
                            .Append("\" ");
                        EmitLocation(p.StartLocation, p.EndLocation, false);
                        EmitId();
                        sb1.Append(')');
                    }
                    sb1.Append(')');
                }
            }

            private void EmitId()
            {
                sb1.Append(" (id ")
                    .Append(Nonce.Instance.Value)
                    .Append(')');
            }

            private void EmitLocation(Location start, Location end)
            {
                EmitLocation(start, end, true);
            }


            private void EmitLocation(Location start, Location end, bool indent)
            {
                if (indent)
                    sb1.Append(new String(' ', 2*depth + 2));

                if (end.Line!=0 || end.Column !=0)
                {
                    sb1.Append("(location (")
                        .Append(start.Line.ToString())
                        .Append(' ')
                        .Append(start.Column.ToString())
                        .Append(") (")
                        .Append(end.Line.ToString())
                        .Append(' ')
                        .Append(end.Column.ToString())
                        .Append("))");
                }
            }


            private void EmitSet<T>(string label, List<T> list, Action<T> emitOne)
            {
                var count = list.Count;
                if (count == 1)
                {
                    emitOne(list[0]);
                }
                else
                {
                    depth+=2;
                    sb1.Append('(')
                        .Append(label)
                        .Append("set nil\n")
                        .Append(new String(' ', 2*depth-2))
                        .Append("(children");
                    foreach (var item in list)
                    {
                        sb1.Append('\n')
                            .Append(new String(' ', 2*depth));
                        emitOne(item);
                        sb1.Append(')');
                    }
                    sb1.Append(")");
                    depth-=2;
                }
            }


            private StringBuilder NodeToStringBuilder(ICSharpCode.NRefactory.Ast.AbstractNode node)
            {
                return NodeToStringBuilder(node, null);
            }



            private void EmitImport (ICSharpCode.NRefactory.Ast.Using u1)
            {
                sb1.Append("(import \"")
                    .Append(u1.Name)
                    .Append('"');
                if (u1.Alias.ToString().Contains("NullTypeReference"))
                    sb1.Append(" nil");
                else
                    sb1.Append(" \'").Append(u1.Alias.ToString()).Append('\'');
            }


            private StringBuilder NodeToStringBuilder(ICSharpCode.NRefactory.Ast.AbstractNode node, string overrideBlock)
            {
                BlockStatement body = null;
                var t = node.GetType();
                bool empty = false;
                if (sb1 == null)
                    sb1 = new StringBuilder();

                depth++;

                sb1.Append(new String(' ', 2*depth));
                if (t == typeof(ICSharpCode.NRefactory.Ast.CompilationUnit))
                {
                    sb1.Append("'(CompilationUnit");
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.UsingDeclaration))
                {
                    var u = node as ICSharpCode.NRefactory.Ast.UsingDeclaration;
                    EmitSet("import", u.Usings, EmitImport);
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.AttributeSection))
                {
                    var a = node as ICSharpCode.NRefactory.Ast.AttributeSection;
                    var emitOne = new Action<ICSharpCode.NRefactory.Ast.Attribute>((a1)=>
                        {
                            sb1.Append("(attribute \"")
                            .Append(a1.Name)
                            .Append("\" ")
                            .Append(a.AttributeTarget);

                            foreach (var pa1 in a1.PositionalArguments)
                            {
                                var pa = pa1 as PrimitiveExpression;
                                if (pa!=null)
                                {
                                    sb1.Append(" \"")
                                        .Append(pa.Value.ToString())
                                        .Append("\" ");
                                }
                            }
                        });
                    EmitSet("attribute", a.Attributes, emitOne);
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.NamespaceDeclaration))
                {
                    var ns = node as ICSharpCode.NRefactory.Ast.NamespaceDeclaration;
                    sb1.Append("(namespace \"");
                    sb1.Append(ns.Name)
                        .Append('\"');
                    //body = ns.Body;
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.TypeDeclaration))
                {
                    var type = node as ICSharpCode.NRefactory.Ast.TypeDeclaration;
                    sb1.Append("(type \"");
                    sb1.Append(type.Name)
                        .Append("\" ");
                    EmitModifier(type);
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ConstructorDeclaration))
                {
                    var ctor = node as ICSharpCode.NRefactory.Ast.ConstructorDeclaration;
                    sb1.Append("(ctor \"");
                    sb1.Append(ctor.Name)
                        .Append("\" ");
                    EmitModifier(ctor);
                    EmitParameters(ctor);
                    body = ctor.Body;
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.MethodDeclaration))
                {
                    var m = node as ICSharpCode.NRefactory.Ast.MethodDeclaration;
                    sb1.Append("(method \"");
                    sb1.Append(m.Name)
                        .Append("\" \"")
                        .Append(m.TypeReference)
                        .Append("\" ");
                    EmitModifier(m);
                    EmitParameters(m);
                    body = m.Body;
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.LocalVariableDeclaration))
                {
                    var lvd = node as ICSharpCode.NRefactory.Ast.LocalVariableDeclaration;
                    var count = lvd.Variables.Count;
                    var emitOne = new Action<VariableDeclaration>((v1) => {
                            sb1.Append("(var \"")
                            .Append(v1.Name)
                            .Append("\" \"")
                            .Append(lvd.TypeReference.ToString())
                            .Append("\" ");
                            //EmitModifier(lvd, sb1, depth);
                        });

                    EmitSet("var", lvd.Variables, emitOne);
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.FieldDeclaration))
                {
                    var f = node as ICSharpCode.NRefactory.Ast.FieldDeclaration;
                    var count =  f.Fields.Count;
                    var emitOne = new Action<VariableDeclaration>((f1) => {
                            sb1.Append("(field \"")
                            .Append(f1.Name)
                            .Append("\" \"")
                            .Append(f.TypeReference)
                            .Append("\" ");
                            EmitModifier(f);
                        });

                    EmitSet("field", f.Fields, emitOne);
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.PropertyDeclaration))
                {
                    var pd = node as ICSharpCode.NRefactory.Ast.PropertyDeclaration;
                    var emitPropGetSet = new Action<PropertyGetSetRegion> ((r) => {
                            depth++;
                            Type t1 = r.GetType();
                            String s = (t1 == typeof(ICSharpCode.NRefactory.Ast.PropertyGetRegion))
                            ? "get" : "set";

                            string indent = new String(' ', 2*depth);
                            sb1.Append('\n')
                            .Append(indent)
                            .Append('(')
                            .Append(s)
                            .Append(' ');
                            if (r.Modifier.ToString() != "None")
                                EmitModifier(r);

                            if (!r.IsNull && (r.Block != BlockStatement.Null))
                            {
                                if (t1 == typeof(ICSharpCode.NRefactory.Ast.PropertySetRegion))
                                {
                                    sb1.Append('\n')
                                        .Append(indent)
                                        .Append("  (params (var \"value\" \"")
                                        .Append(pd.TypeReference)
                                        .Append("\" ");
                                    EmitLocation(r.StartLocation, r.EndLocation, false);
                                    EmitId();
                                    sb1.Append(")) ");
                                }

                                sb1.Append('\n');
                                NodeToStringBuilder(r.Block);
                                sb1.Append('\n');
                                EmitLocation(r.Block.StartLocation, r.Block.EndLocation);
                                EmitId();
                            }
                            depth--;
                            sb1.Append(')');
                        });

                    sb1.Append("(property \"")
                        .Append(pd.Name)
                        .Append("\" \"")
                        .Append(pd.TypeReference)
                        .Append("\" ");
                    EmitModifier(pd);

                    if (pd.HasGetRegion)
                        emitPropGetSet(pd.GetRegion);

                    if (pd.HasSetRegion)
                        emitPropGetSet(pd.SetRegion);

                    // xxx
                    //sb1.Append('\n');
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.TryCatchStatement))
                {
                    var tc = node as ICSharpCode.NRefactory.Ast.TryCatchStatement;
                    sb1.Append("(trycatch\n");
                    //sb1.Append(tc.StatementBlock.ToString());
                    NodeToStringBuilder(tc.StatementBlock, "(try");
                    foreach (var cc1 in tc.CatchClauses)
                    {
                        var cc = cc1 as CatchClause;
                        if (cc!=null)
                        {
                            sb1.Append('\n')
                                .Append(new String(' ', 2*depth + 2))
                                .Append("(catch (var \"")
                                .Append(cc.VariableName)
                                .Append("\" \"")
                                .Append(cc.TypeReference)
                                .Append("\" ");
                            EmitLocation(cc.StartLocation, cc.EndLocation, false);
                            EmitId();
                            sb1.Append(")\n");
                            NodeToStringBuilder(cc.StatementBlock, "");
                            sb1.Append(')');
                        }
                    }
                    if (tc.FinallyBlock as BlockStatement != null)
                    {
                        sb1.Append('\n');
                        NodeToStringBuilder(tc.FinallyBlock, "(finally");
                    }
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.BlockStatement))
                {
                    // var bs = node as ICSharpCode.NRefactory.Ast.BlockStatement;
                    //    children will be emitted below.
                    //    remove the indent that was added above.
                    if (!String.IsNullOrEmpty(overrideBlock))
                        sb1.Append(overrideBlock);
                    else if (overrideBlock != "")
                        sb1.Append("(block");
                    else
                    {
                        sb1.Length -= 2*depth+1;
                        empty = true;
                    }
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ExpressionStatement))
                {
                    // then or else
                    if (!String.IsNullOrEmpty(overrideBlock))
                        sb1.Append(overrideBlock);
                    else
                        sb1.Append("(expression ");
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ReturnStatement))
                {
                    sb1.Append("(return ");
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.IfElseStatement))
                {
                    var ifelse = node as ICSharpCode.NRefactory.Ast.IfElseStatement;
                    sb1.Append("(if ");
                    //sb1.Append(node.ToString());
                    foreach (var s in ifelse.TrueStatement)
                    {
                        sb1.Append('\n');
                        NodeToStringBuilder(s, "(then");
                    }
                    if (ifelse.HasElseIfSections)
                    {
                        foreach (var s in ifelse.ElseIfSections)
                        {
                            sb1.Append('\n');
                            NodeToStringBuilder(s.EmbeddedStatement, "(elseif");
                        }
                    }
                    if (ifelse.HasElseStatements)
                    {
                        foreach (var s in ifelse.FalseStatement)
                        {
                            //sb1.Append("++\n");
                            sb1.Append('\n');
                            NodeToStringBuilder(s, "(else");
                        }
                    }
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ForeachStatement))
                {
                    var stmt = node as ICSharpCode.NRefactory.Ast.ForeachStatement;
                    var expr = stmt.Expression;

                    sb1.Append("(foreach (var \"")
                        .Append(stmt.VariableName)
                        .Append("\" \"")
                        .Append(stmt.TypeReference.ToString());
                    sb1.Append("\" ");
                    // location of the variable initialization
                    EmitLocation(expr.StartLocation, expr.EndLocation, false);
                    // xxx
                    //EmitLocation(stmt.StartLocation, expr.EndLocation, false);
                    EmitId();
                    sb1.Append(")\n");

                    //sb1.Append(node.ToString());
                    NodeToStringBuilder(stmt.EmbeddedStatement);
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ForStatement))
                {
                    var fs = node as ICSharpCode.NRefactory.Ast.ForStatement;
                    sb1.Append("(for \n");
                    foreach (var stmt in fs.Initializers)
                    {
                        NodeToStringBuilder(stmt);
                    }
                    sb1.Append("\n");
                    NodeToStringBuilder(fs.EmbeddedStatement);
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.SwitchStatement))
                {
                    var ss = node as ICSharpCode.NRefactory.Ast.SwitchStatement;
                    sb1.Append("(switch ");
                    //sb1.Append(node.ToString());
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.ThrowStatement))
                {
                    //var ts = node as ICSharpCode.NRefactory.Ast.ThrowStatement;
                    sb1.Append("(throw ");
                }
                else if (t == typeof(ICSharpCode.NRefactory.Ast.UsingStatement))
                {
                    var us = node as ICSharpCode.NRefactory.Ast.UsingStatement;
                    sb1.Append("(using \n");
                    NodeToStringBuilder(us.ResourceAcquisition);
                    sb1.Append("\n");
                    NodeToStringBuilder(us.EmbeddedStatement);
                }
                else
                {
                    // all others
                    sb1.Append("(")
                        .Append(t.ToString())
                        .Append(" ")
                        .Append(node.ToString());
                }


                if (node.Children != null &&
                    ((t == typeof(ICSharpCode.NRefactory.Ast.BlockStatement)) ||
                     node.Children.Count > 0))
                {
                    int count = node.Children.Count;
                    int n = 0;

                    if (t != typeof(ICSharpCode.NRefactory.Ast.BlockStatement))
                    {
                        sb1.Append('\n')
                            .Append(new String(' ', 2*depth+2))
                            .Append("(children");
                        depth++;
                    }
                    foreach (var c in node.Children)
                    {
                        if (n == 0)
                            sb1.Append('\n');
                        NodeToStringBuilder(c as AbstractNode);
                        n++;
                        if (n < count)
                            sb1.Append('\n');
                    }
                    if (count == 0)
                        sb1.Append(' ');
                    if (t != typeof(ICSharpCode.NRefactory.Ast.BlockStatement))
                    {
                        sb1.Append(")");
                        depth--;
                    }
                }

                if (t != typeof(ICSharpCode.NRefactory.Ast.CompilationUnit))
                {
                    sb1.Append('\n');
                    //sb1.Append("--\n"); // debugging only

                    // some structures have a body - like methods, ctors, etc.
                    if (body != null)
                    {
                        // emit body as well
                        NodeToStringBuilder(body as AbstractNode);

                        //sb1.Append("::\n")
                        sb1.Append('\n');
                        EmitLocation(node.StartLocation, body.EndLocation);
                    }

                    // special case properties, because they are special!
                    else if (t == typeof(ICSharpCode.NRefactory.Ast.PropertyDeclaration))
                    {
                        var p = node as ICSharpCode.NRefactory.Ast.PropertyDeclaration;
                        EmitLocation(node.StartLocation, p.BodyEnd);
                    }

                    else
                    {
                        EmitLocation(node.StartLocation, node.EndLocation);
                    }
                }

                if (!empty)
                {
                    EmitId();
                    sb1.Append(')');
                }
                depth--;
                return sb1;
            }
        }
    }


}




/*



  [Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl(' var foo = "this is a string"; var fred = new System.Collections.Generic.List<String> { foo, foo.Length.ToString() }; var z = fred.Count; var mmm = count + 8; var nnn = mmm.ToString() + this.InstanceMethod1(count);','System,System.Xml,System.IO','',4,'CsharpCompletion','int count','private static int staticField1 = default(int); string InstanceMethod1(int index) { return default(string);} ')


  [Ionic.Cscomp.Utilities]::GetConstructors("System.String");

  [Ionic.Cscomp.Utilities]::GetConstructors("System.DateTimeKind");

  [Ionic.Cscomp.Utilities]::GetMatches("DateT", "System,System.IO");



  [Ionic.Cscomp.Utilities]::QualifyType("System.Collections.Generic.List`1");

  [Ionic.Cscomp.Utilities]::GetTypeInfo("System.Collections.Generic.List");

  [Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl('var x = String.Format("{0}", 928);' )

  [Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl('var x = new System.Collections.Generic.List<string>();')



  [Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl(' var doo = "Tra la la"; var fred = new System.Collections.Generic.List<String>
  {
  doo.Length.ToString()
  };');

  [Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl(' var doo = "Tra la la"; var fred = new System.Collections.Generic.List<String> { doo.Length.ToString() };', $null, $null, 2);



  [Ionic.Cscomp.Utilities]::GetTypeInfo("System.Data.SqlClient.SqlCommand","System.Data, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");

  [Ionic.Cscomp.Utilities]::GetTypeInfo("System.Xml.XmlReader","System.Xml, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");


  [Ionic.Cscomp.Utilities]::GetTypeInfo("System.DateTime");

  [Ionic.Cscomp.Utilities]::QualifyType("System.DateTime");

  [Ionic.Cscomp.Utilities]::QualifyType("XmlSerializer");

  [Ionic.Cscomp.Utilities]::QualifyType("System.Xml.XmlReader");

  [Ionic.Cscomp.Utilities]::QualifyType("System.This.Type.Does.Not.Exist");


  [Ionic.Cscomp.Utilities]::LoadOneAssembly("Ionic.Zip.dll");

  [Ionic.Cscomp.Utilities]::LoadOneAssembly("System.Xml");

  [Ionic.Cscomp.Utilities]::QualifyName("System.Xml");

  [Ionic.Cscomp.Utilities]::GetCompletionsForNamespace("System.Collections.Generic")

  [Ionic.Cscomp.Utilities]::ListLoadedAssemblies();


  [Ionic.Cscomp.Utilities]::ListKnownTypes();



  unnecessary, because these are all built-in to the DLL
  [Ionic.Cscomp.Utilities]::SetAssemblySearchPaths('c:\.net2.0,c:\net3.0ra,c:\.net3.5ra');

  [Ionic.Cscomp.Utilities]::Version();
  [Ionic.Cscomp.Utilities]::Verbose = $TRUE


  [System.Reflection.Assembly]::LoadFrom("c:\\users\\dino\\elisp\\cscomp\\CscompUtilities.dll");



*/


