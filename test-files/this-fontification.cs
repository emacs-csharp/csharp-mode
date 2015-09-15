using System;

namespace DemoSpace
{
    public class DemoClass
    {
        private InnerClass _innerClass;

        public DemoClass()
        {
            // note usage of plain "this".
            _innerClass = new InnerClass(this);
        }
    }

    public static class DemoClassExtensions
    {
        public static bool ExampleWithMemberType(this DemoClass instance, string foo)
        {
            // fontifies parameters incorrectly
            return false;
        }

        public static bool ExampleWithBulitInType(this string instance, int bar)
        {
            // fontifies parameters correctly
            return true;
        }
    }
}
