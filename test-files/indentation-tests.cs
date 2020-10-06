using System;

/* comment block
   on namespace test */
namespace Boo
{
    // comment on class test
    public class Foo
    {
        // auto-property-test
        public bool AutoProperty { get; set; }

        // regular property-test
        public bool Property
        {
            get
            {
                return true;
            }
            set
            {
                // ignored
            }
        }

        /// <summary>
        ///   Codedoc on method-test
        /// </summary>
        public void Foo(string a = "hkfdhkd", string b = "bbbbbb")
        {
            // OK!
        }

        public void Test()
        {
            if (test)
            {

            }

            if (test2) {
                // should work too
                bool b = true;
            }

            var x = new {
                adhoc = object,
                with = new prop(),
            };

            // test-cases for type initializer indetation issue:
            // https://github.com/josteink/csharp-mode/issues/95
            var x_gen = new Foo<bar>
            {
                a,
                b,
                c
            };

            return new Foo<bar>
            {
                a,
                b,
                c
            };

            yield return new Foo<bar>
            {
                a,
                b,
                c
            };

            new Foo<Bar>
            {
                a,
                b,
                c
            };

            var array1 = new ArrayList
            {
                1, 2, 3, 4, 5
            };

            var array2 = new string[]
            {
                "a", "b", "c"
            };

            var map = new Dictionary<int,string> {
                { 1, "true" },
                { 2, "false" },
            };

            var map2 = new Dictionary<int,string>
            {
                { 1, "true" },
                { 2, "false" },
            };

            var map3 = new Dictionary<string,Func<int, bool>>
            {
                { "IsZero", (int i) => i == 0 }
            };

            var innerClassIntance = new TypeOuter.TypeInner
            {
                Boo = "Foo",
                May = "Yay"
            };

            // yielding has different behaviour... for some reason!
            // https://github.com/josteink/csharp-mode/issues/94
            yield return new InnerA {
                PropA = 1,
                PropB = 2
            };

            // Commented out for rework -- Theodor Thornhill
            // yield return new InnerA.InnerB {
            //     PropA = 1,
            //     PropB = 2
            // };

            yield return new InnerA
            {
                Boo = "Foo",
                May = "Yay"
            };

            // Commented out for rework -- Theodor Thornhill
            // yield return new InnerA.InnerB
            // {
            //     Boo = "Foo",
            //     May = "Yay"
            // };

            // extra test-cases

            new Foo{
                a,
                b,
                c
            };

            new Foo  {
                a,
                b,
                c
            };

            new Foo
            {
                a,
                b,
                c
            };

            using (test)
            {
                System.Console.WriteLine("boo");
            }

            /* Callback indentation test. */
            SomeFunction(() => {
                System
                    .Console
                    .WriteLine("boo");
            });

            SomeFunction(() =>
            {
                System
                    .Console
                    .WriteLine("boo");
            });

            SomeFunction((withParam) => {
                System
                    .Console
                    .WriteLine("boo");
            });

            SomeFunction((withParam) =>
            {
                System
                    .Console
                    .WriteLine("boo");
            });
        }

        public void CaseStamentIndentation()
        {
            int bar = 0;
            switch (foo)
            {
                case "foo":
                    bar = 0;
                    break;
            }
            switch (bar)
            {
                case 1:
                case 2:
                    bar = 0;
                    break;
            }
            switch (foo)
            {
                case "foo":
                case "bar":
                    bar = 0;
                    bar += 1;
                    break;
            }
        }
    }
}

public class NestedPreProcessor
{
#if DEBUG
    public static bool Debug = true;
#endif
}

enum SomeEnum
{
    Item,
    Another,
    Yeah
}

enum AnotherEnum
{
    First = 1,
    Second = 2,
    Third = 3
}

public async Task WriteAsync()
{
    using (var memoryStream = new MemoryStream())
    using (var writer = new BinaryWriter())
    {
        // double using should indent like this
    }

    using (var memoryStream = new MemoryStream())
    {
        using (var writer = new BinaryWriter())
        {
            // or this
        }
    }
}
