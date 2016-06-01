using System;

/// <summary>
///   This class is designed to support a test-case for imenu indexing
///   of a class' member fields.
/// </summary>
public class FieldTestClass
{
    public bool TestBool;

    private string CommentedField; // this is a comment

    private static int _MultiLineComment; /*
                                        new line */

    internal volatile DateTime VolatileTest;
}
