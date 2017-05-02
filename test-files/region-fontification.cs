using System;

public class Test
{
    public void Test()
    {
        #region fontifies as a region comment
        string foo = "bar";
        #endregion
        
        #region quotes shouldn't mess up a region comment
        string foo = "bar";
        #endregion
        
        #region   any number of spaces are allowed before a region comment
        string foo = "bar";
        #endregion
        
        #region	a TAB is also allowed before a region comment
        string foo = "bar";
        #endregion

        #  region a region comment is fine when there are spaces between the # and the region
        string foo = "bar";
        #  endregion
        
        #	region a TAB between the # and the region doesn't stop this being a region comment either
        string foo = "bar";
        #	endregion

        #region --- a region comment doesn't have to start with a word ---
        string foo = "bar";
        #endregion

        #region "a region comment can even look like this"
        string foo = "bar";
        #endregion

        // This is here just to ensure the commentification doesn't bleed to the next line
        #region
        string foo = "bar";
        #endregion
    }
}
