using System;

class ImenuMatcherTests
{
    private void OneGeneric<T>(this IAppBuilder builder, params object[] args)
    {
        Console.WriteLine("TODO: something");
    }

    private void TwoGeneric<T1,T2>(this IAppBuilder builder, params object[] args)
    {
        Console.WriteLine("TODO: something");
    }

    private void NoGeneric(this IAppBuilder builder, params object[] args)
    {
        Console.WriteLine("TODO: something");
    }
}
