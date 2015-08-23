using System;

public class Test
{
    public void Test()
    {
        string x;

        // reference
        x += "foo";
        #region v1 verification
        x += "foo";
        #endregion

        #region t1 test'
        x += "foo";
        #endregion

        #region t2 - test"
        x += "foo";
        #endregion
    }
}
