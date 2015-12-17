using System;

public interface IImenuTest
{
    string InterfaceString { get; }
}

public class ImenuTest : IImenuTest
{
    string IImenuTest.InterfaceString { get { return "i"; }}
}
