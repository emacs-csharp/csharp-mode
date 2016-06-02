using System;

public class Test
{
    public void Test()
    {
        string x;

        // reference
        x += "strReference";
        #region v1 verification
        x += "strVerification";
        #endregion

        #region t1 test'
        x += "singleQuote";
        #endregion

        #region t2 - test"
        x += "doubleQuote";
        #endregion
    }
}
