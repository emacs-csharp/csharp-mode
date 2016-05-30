using System;

public interface IImenuTest
{
    string InterfaceString { get; }
}

/// <summary>
///   This test-case checks whether imenu indexes member declarations
///   with no access-modifier, but instead with the explicit
///   interface-name prefixed.
///
///   We expect both the method and property to show up in our index,
///   with interface-name included.
/// </summary>
public class ImenuTest : IImenuTest
{
    string IImenuTest.InterfaceString { get { return "i"; }}

    string IImenuTest.MethodName(string param1, int param2)
    {

    }
}
