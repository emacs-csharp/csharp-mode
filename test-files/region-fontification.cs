using System;

public class Test
{
    public void Test()
    {
        #region fontifies correctly
        string foo = "bar";
        #endregion
        
        #region does'nt fontify correctly
        string foo = "bar";
        #endregion
    }
}