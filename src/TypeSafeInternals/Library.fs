namespace TheAngryByrd.TypeSafeInternals
open System
open System.Reflection
module Delegate =
    let public BindingFlagsToSeeAll : BindingFlags=
            BindingFlags.Static |||
            BindingFlags.FlattenHierarchy |||
            BindingFlags.Instance |||
            BindingFlags.NonPublic |||
            BindingFlags.Public;


    let createStaticArity0<'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'returnType>>)
        |> unbox<Func<'returnType>>
        |> FuncConvert.FromFunc

    let createStaticArity1<'input1, 'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'input1, 'returnType>>)
        |> unbox<Func<'input1, 'returnType>>
        |> FuncConvert.FromFunc

    let createStaticArity2<'input1, 'input2, 'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'input1, 'input2, 'returnType>>)
        |> unbox<Func<'input1, 'input2, 'returnType>>
        |> FuncConvert.FromFunc

    let createStaticArity3<'input1, 'input2, 'input3, 'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'input1, 'input2, 'input3, 'returnType>>)
        |> unbox<Func<'input1, 'input2, 'input3, 'returnType>>
        |> FuncConvert.FromFunc

    let createStaticArity4<'input1, 'input2, 'input3, 'input4, 'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'input1, 'input2, 'input3, 'input4, 'returnType>>)
        |> unbox<Func<'input1, 'input2, 'input3, 'input4, 'returnType>>
        |> FuncConvert.FromFunc

    let createStaticArity5<'input1, 'input2, 'input3, 'input4, 'input5, 'returnType> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Func<'input1, 'input2, 'input3, 'input4, 'input5, 'returnType>>)
        |> unbox<Func<'input1, 'input2, 'input3, 'input4, 'input5, 'returnType>>
        |> FuncConvert.FromFunc



    let createStaticArity0ReturningUnit (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action>)
        |> unbox<Action>
        |> FuncConvert.FromAction

    let createStaticArity1ReturningUnit<'input1> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action<'input1>>)
        |> unbox<Action<'input1>>
        |> FuncConvert.FromAction

    let createStaticArity2ReturningUnit<'input1, 'input2> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action<'input1, 'input2>>)
        |> unbox<Action<'input1, 'input2>>
        |> FuncConvert.FromAction

    let createStaticArity3ReturningUnit<'input1, 'input2, 'input3> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action<'input1, 'input2, 'input3>>)
        |> unbox<Action<'input1, 'input2, 'input3>>
        |> FuncConvert.FromAction

    let createStaticArity4ReturningUnit<'input1, 'input2, 'input3, 'input4> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action<'input1, 'input2, 'input3, 'input4>>)
        |> unbox<Action<'input1, 'input2, 'input3, 'input4>>
        |> FuncConvert.FromAction

    let createStaticArity5ReturningUnit<'input1, 'input2, 'input3, 'input4, 'input5> (ty : Type) name =
        ty.GetMethod(name, BindingFlagsToSeeAll)
            .CreateDelegate(typeof<Action<'input1, 'input2, 'input3, 'input4, 'input5>>)
        |> unbox<Action<'input1, 'input2, 'input3, 'input4, 'input5>>
        |> FuncConvert.FromAction
