﻿namespace TheAngryByrd.Myriad.Plugins.TypeSafeInternals

open Myriad.Core
open System.Reflection
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core.Ast
open FSharp.Compiler.Range
open System
open FSharp.Compiler.XmlDoc
open Microsoft.FSharp.Reflection

module Debugging =

    let mutable shouldBreak = true

    let waitForDebuggerAttached (programName) =
#if DEBUG
        if not (Diagnostics.Debugger.IsAttached) then
            printfn
                "Please attach a debugger for %s, PID: %d"
                programName
                (Diagnostics.Process.GetCurrentProcess().Id)

        while not (Diagnostics.Debugger.IsAttached) do
            System.Threading.Thread.Sleep(100)

        if shouldBreak then
            Diagnostics.Debugger.Break()
            shouldBreak <- false
#else
        ()
#endif

[<AutoOpenAttribute>]
module Extensions =
    type SynPatRcd with

        static member CreateNamed(id, pattern, access) =
            SynPatRcd.Named
                {
                    Pattern = pattern
                    Id = id
                    IsThis = false
                    Access = Some access
                    Range = range.Zero
                }

        static member CreateLongIdent(id, args, access) =
            SynPatRcd.LongIdent
                {
                    Args = args
                    Id = id
                    Access = Some access
                    Range = range.Zero
                    ExtraId = None
                    TyparDecls = None
                }

        static member CreateUnit() =
            SynPatRcd.CreateParen(
                SynPatRcd.Const
                    {
                        Const = SynConst.Unit
                        Range = range.Zero
                    }
            )

    type SynSimplePat with

        static member CreateId(ident) =
            SynSimplePat.Id(ident, None, false, false, false, range0)

    type SynSimplePats with

        static member CreateSimplePats(pats) = SynSimplePats.SimplePats(pats, range0)

    type SynExpr with

        static member CreateTypeApp expr generics =
            SynExpr.TypeApp(expr, range0, generics, [], None, range0, range0)

        static member CreateLambda(args, body, parsedData) =
            SynExpr.Lambda(false, false, args, body, parsedData, range0)

    type SynType with

        static member CreateApp(typeName, typeArgs) =
            SynType.App(typeName, None, typeArgs, [], None, false, range0)

        static member CreateArray(elementType) = SynType.Array(1, elementType, range0)

        static member CreateArray(elementType, rank) =
            SynType.Array(rank, elementType, range0)

        static member CreateParen(innerType) = SynType.Paren(innerType, range0)

        static member CreateTuple(elementTypes) =
            SynType.Tuple(false, elementTypes, range0)

        static member CreateStructTuple(elementTypes) =
            SynType.Tuple(true, elementTypes, range0)

        static member CreateVar(typar) = SynType.Var(typar, range0)

module String =
    let removeEnd (toRemove: string) (theString: string) =
        if theString.EndsWith(toRemove) then
            theString.Substring(
                0,
                theString.Length
                - toRemove.Length
            )
        else
            theString

module TypeHelpers =

    let replacePlusWithDotInNestedTypeName (s: string) = s.Replace("+", ".")

    let replaceModuleFullName (s: Type) =
        if
            s
            |> FSharpType.IsModule
        then
            s.FullName
            |> String.removeEnd "Module"
        else
            s.FullName

    let isNamedType (typ: Type) =
        not (
            typ.IsArray
            || typ.IsByRef
            || typ.IsPointer
        )

    let equivHeadTypes (ty1: Type) (ty2: Type) =
        isNamedType ty1
        && if ty1.IsGenericType then
               ty2.IsGenericType
               && (ty1.GetGenericTypeDefinition()).Equals(ty2.GetGenericTypeDefinition())
           else
               ty1.Equals ty2


    let isOptionType typ = equivHeadTypes typ (typeof<int option>)

    let isFunctionType typ =
        equivHeadTypes typ (typeof<(int -> int)>)

    let isListType typ = equivHeadTypes typ (typeof<int list>)

    let rec getFSTypeName (t: Type) =

        if t = typeof<string> then
            SynType.CreateLongIdent "string", []
        elif t = typeof<Void> then
            SynType.CreateLongIdent "unit", []
        elif t = typeof<obj> then
            SynType.CreateLongIdent "obj", []
        elif t = typeof<char> then
            SynType.CreateLongIdent "char", []
        elif t = typeof<int16> then
            SynType.CreateLongIdent "int16", []
        elif t = typeof<int32> then
            SynType.CreateLongIdent "int32", []
        elif t = typeof<int64> then
            SynType.CreateLongIdent "int64", []
        elif t = typeof<uint16> then
            SynType.CreateLongIdent "uint16", []
        elif t = typeof<uint32> then
            SynType.CreateLongIdent "uint32", []
        elif t = typeof<uint64> then
            SynType.CreateLongIdent "uint64", []
        elif t = typeof<float> then
            SynType.CreateLongIdent "float", []
        elif t = typeof<float32> then
            SynType.CreateLongIdent "float32", []
        elif t = typeof<single> then
            SynType.CreateLongIdent "single", []
        elif t = typeof<double> then
            SynType.CreateLongIdent "dpuble", []
        elif t = typeof<decimal> then
            SynType.CreateLongIdent "decimal", []
        elif t.IsGenericMethodParameter then
            let synTy =
                Typar(Ident.Create t.Name, NoStaticReq, false)
                |> SynType.CreateVar

            synTy, [ t.Name ]
        elif FSharpType.IsFunction t then
            let domain, range = FSharpType.GetFunctionElements t
            let (arg1, generics1) = getFSTypeName domain
            let arg2, generics2 = getFSTypeName range
            let synTy = SynType.CreateParen(SynType.CreateFun(arg1, arg2))

            synTy,
            (generics1
             @ generics2)
        elif FSharpType.IsTuple t then
            let types =
                t.GetGenericArguments()
                |> Array.map (fun t -> getFSTypeName t)
                |> Array.toList

            let synTy =
                types
                |> List.map (fun (synTy, _) -> false, synTy)
                |> SynType.CreateTuple

            synTy,
            types
            |> List.collect (fun (_, gs) -> gs)
        elif t.IsGenericType then
            toGenericTypeString t
        elif t.IsArray then
            let synTy, gs =
                t.GetElementType()
                |> getFSTypeName

            (SynType.CreateArray synTy), gs
        else if
            t.DeclaringType
            <> null
            && t.DeclaringType
               |> FSharpType.IsModule
        then

            let synTy, gs =
                if
                    t.DeclaringType.CustomAttributes
                    |> Seq.exists (fun a ->
                        a.AttributeType = typeof<RequireQualifiedAccessAttribute>
                    )
                then
                    SynType.CreateLongIdent $"{replaceModuleFullName t.DeclaringType}.{t.Name}", []
                else
                    SynType.CreateLongIdent t.FullName, []

            synTy, gs
        else
            let synTy =
                t.FullName
                |> replacePlusWithDotInNestedTypeName
                |> SynType.CreateLongIdent

            synTy, []

    and toFSReservatedWord (fullName) (t: Type) =
        if t = typeof<string> then
            "string"
        elif t = typeof<Void> then
            "unit"
        elif t = typeof<obj> then
            "obj"
        elif t = typeof<char> then
            "char"
        elif t = typeof<int16> then
            "int16"
        elif t = typeof<int32> then
            "int32"
        elif t = typeof<int64> then
            "int64"
        elif t = typeof<uint16> then
            "uint16"
        elif t = typeof<uint32> then
            "uint32"
        elif t = typeof<uint64> then
            "uint64"
        elif t = typeof<float> then
            "float"
        elif t = typeof<float32> then
            "float32"
        elif t = typeof<single> then
            "single"
        elif t = typeof<double> then
            "dpuble"
        elif t = typeof<decimal> then
            "decimal"
        elif
            t
            |> isOptionType
        then
            "option"
        elif
            t
            |> isListType
        then
            "list"
        elif
            fullName
            && t.FullName
               <> null
        then
            t.FullName
        else
            t.Name

    and toGenericTypeString (t: Type) =
        let typeArgs =
            t.GetGenericArguments()
            |> Seq.map (getFSTypeName)
            |> Seq.toList

        let generics =
            typeArgs
            |> List.collect (fun (_, gs) -> gs)

        let synTy =
            SynType.CreateApp(
                (toFSReservatedWord false t
                 |> SynType.CreateLongIdent),
                typeArgs
                |> List.map (fun (ts, _) -> ts)
            )

        synTy, generics


module DSL =
    /// Creates : open {{namespace}}
    let openNamespace (``namespace``) =
        SynOpenDeclTarget.ModuleOrNamespace(``namespace``, range.Zero)
        |> SynModuleDecl.CreateOpen


    let private equalIdent = Ident.Create "op_Equality"

    let equalsSign synExpr2 synExpr1 =
        SynExpr.CreateApp(
            SynExpr.CreateAppInfix(SynExpr.CreateIdent equalIdent, synExpr1),
            synExpr2
        )

    let (!=!) synExpr1 synExpr2 =
        synExpr1
        |> equalsSign synExpr2


    let private pipeRightIdent = Ident.Create "op_PipeRight"

    /// Creates : {{synExpr1}} |> {{synExpr2}}
    let pipeRight synExpr2 synExpr1 =
        SynExpr.CreateApp(
            SynExpr.CreateAppInfix(SynExpr.CreateIdent pipeRightIdent, synExpr1),
            synExpr2
        )

    let createLongIdentWithDots str =
        (LongIdentWithDots.CreateString str).Lid

    /// Creates : let {{leftSide}} = {{rightSide}}
    ///
    /// A more concrete example: let myVar = "something"
    let createLetAssignment leftSide rightSide continuation =
        let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
        let headPat = SynPat.Named(SynPat.Wild range0, leftSide, false, None, range0)

        let binding =
            SynBinding.Binding(
                None,
                SynBindingKind.NormalBinding,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                emptySynValData,
                headPat,
                None,
                rightSide,
                range0,
                DebugPointForBinding.DebugPointAtBinding range0
            )

        SynExpr.LetOrUse(false, false, [ binding ], continuation, range0)


    let createTypeOf (ty: SynType) =
        let typeOf = SynExpr.CreateIdentString "typeof"
        SynExpr.CreateTypeApp typeOf [ ty ]

module DSLOperators =
    ///Infix for SynExpr.CreateApp(funcExpr, argExpr)
    let (<@>) funcExpr argExpr = SynExpr.CreateApp(funcExpr, argExpr)


open DSLOperators
open NuGet.ProjectModel


type ReflectedModuleName = ReflectedModuleName of string
type ProCompiledModuleName = ProCompiledModuleName of string


[<RequireQualifiedAccessAttribute>]
type ModuleTree =
    | Module of string * ModuleTree list
    | Leaf of list<SynModuleDecl>

module ModuleTree =

    let fromExtractRecords (xs: list<Ident list * list<SynModuleDecl>>) =
        let rec addPath subFilePath parts nodes =
            match parts with
            | [] -> nodes
            | hp :: tp -> addHeadPath subFilePath hp tp nodes

        and addHeadPath subFilePath (part: string) remainingParts (nodes: ModuleTree list) =
            match nodes with
            | [] ->
                let classes =
                    if
                        remainingParts
                        |> List.isEmpty
                    then
                        (snd subFilePath)
                        |> List.map (fun c -> ModuleTree.Leaf(c))
                    else
                        List.empty

                ModuleTree.Module(part, addPath subFilePath remainingParts classes)
                |> List.singleton
            | ModuleTree.Module (title, subnodes) :: nodes when title = part ->
                ModuleTree.Module(title, addPath subFilePath remainingParts subnodes)
                :: nodes
            | hn :: tn ->
                hn
                :: addHeadPath subFilePath part remainingParts tn

        ([], xs)
        ||> List.fold (fun state (moduleIdent, synTypeDefns) ->
            let pathParts =
                moduleIdent
                |> List.map (fun i -> i.idText)

            addPath (moduleIdent, [ synTypeDefns ]) pathParts state
        )


type ModuleInfos = {
    Type: Type
    StaticMethods: MethodInfo list
    VisibleInternalRecords: Type list
}

type ToGenerate = {
    Assembly: Assembly
    ModuleInfos: ModuleInfos list
}

[<MyriadGenerator("theangrybyrd.typesafeinternals")>]
type TypeSafeInternalsGenerator() =
    let bindingFlagsToSeeAll: BindingFlags =
        BindingFlags.Static
        ||| BindingFlags.FlattenHierarchy
        ||| BindingFlags.Instance
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Public

    interface IMyriadGenerator with

        member x.ValidInputExtensions = seq { ".txt" }

        member x.Generate(ctx: GeneratorContext) =
            let outputDir = System.Environment.CurrentDirectory

            let projectAssetsJsonFileInfo =
                IO.FileInfo
                <| IO.Path.Combine(outputDir, "obj", "project.assets.json")

            let lockfileFormat = LockFileFormat()
            let lockfile = lockfileFormat.Read(projectAssetsJsonFileInfo.FullName)

            let findDLL assemblyName =
                lockfile.Targets
                |> Seq.collect (fun t -> t.Libraries)
                |> Seq.tryFind (fun l -> l.Name = assemblyName)
                |> Option.map (fun l ->
                    let lib =
                        lockfile.Libraries
                        |> Seq.find (fun l -> l.Name = assemblyName)

                    l.CompileTimeAssemblies
                    |> Seq.collect (fun c ->
                        lockfile.PackageFolders
                        |> Seq.map (fun p -> IO.Path.Combine(p.Path, lib.Path, c.Path))
                        |> Seq.filter (fun f -> IO.File.Exists f)
                    )
                )
                |> Option.defaultValue Seq.empty

            let parseVersion (v: string) = v.Split('=').[1]

            AppDomain.CurrentDomain.add_AssemblyResolve (
                ResolveEventHandler(fun _ args ->
                    if args.Name.Contains(".resources") then
                        null
                    else
                        match
                            AppDomain.CurrentDomain.GetAssemblies()
                            |> Seq.tryFind (fun a -> a.FullName = args.Name)
                        with
                        | Some a -> a
                        | None ->
                            // For example: Npgsql, Version=4.1.1.0, Culture=neutral, PublicKeyToken=5d8b90d52f46fda7
                            let toLoad =
                                match
                                    args.Name.Split(',')
                                    |> Array.toList
                                with
                                | name :: version :: _ -> {|
                                    Name = name
                                    Version = parseVersion version
                                  |}
                                | others -> failwithf "None match %A" others

                            match
                                findDLL toLoad.Name
                                |> Seq.tryHead
                            with
                            | Some lib -> Assembly.LoadFrom lib
                            | None -> null
                )
            )

            let assemblies =
                ctx.InputFilename
                |> IO.File.ReadAllLines
                |> Seq.toList

            let moduleFilterTypes = [
                "+"
                "$"
            ]

            let functionFilterTypes = [
                "@"
                "|"
            ]

            let infos = [
                for assemblyName in assemblies do
                    let assembly = Assembly.Load assemblyName
                    let types = assembly.GetTypes()

                    let cleanTys =
                        types
                        |> Seq.filter (fun t ->
                            moduleFilterTypes
                            |> Seq.exists t.FullName.Contains
                            |> not
                        )
                        |> Seq.filter (fun t ->
                            t
                            |> FSharpType.IsModule
                        )

                    let moduleInfos = [
                        for ty in cleanTys do
                            let staticMethods =
                                ty.GetMethods(bindingFlagsToSeeAll)
                                |> Array.filter (fun mi ->
                                    functionFilterTypes
                                    |> List.exists mi.Name.Contains
                                    |> not
                                )
                                |> Array.filter (fun mi ->
                                    mi.IsPublic
                                    |> not
                                    && mi.IsStatic
                                )
                                |> Array.toList

                            let visibleInternalRecords =
                                ty.GetNestedTypes()
                                |> Array.filter (fun ty ->
                                    Reflection.FSharpType.IsRecord(ty, BindingFlags.NonPublic)
                                    && ty.IsVisible
                                )
                                |> Array.toList

                            yield {
                                Type = ty
                                StaticMethods = staticMethods
                                VisibleInternalRecords = visibleInternalRecords
                            }
                    ]

                    yield {
                        Assembly = assembly
                        ModuleInfos = moduleInfos
                    }
            ]

            let ``let private loadedAssembly = Assembly.Load`` assemblyName =
                let funcExpr =
                    SynExpr.CreateLongIdent
                    <| LongIdentWithDots.CreateString("System.Reflection.Assembly.Load")

                let argExpr =
                    SynExpr.CreateParenedTuple [
                        SynExpr.CreateConst
                        <| SynConst.CreateString assemblyName
                    ]

                let leftHandSize =
                    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString("loadedAssembly"), [])

                let rightHandSide =
                    funcExpr
                    <@> argExpr

                let letBinding =
                    { SynBindingRcd.Let with
                        Expr = rightHandSide
                        Pattern = leftHandSize
                        Access = Some SynAccess.Private
                    }

                SynModuleDecl.CreateLet [ letBinding ]

            let ``let private sqlmodule = loadedAssembly.GetTypes() |> Seq.find(fun t -> t.FullName =``
                (ReflectedModuleName moduleName)
                =
                let leftHandSize =
                    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString("loadedModule"), [])

                let rightHandSide =
                    let instanceAndMethod =
                        LongIdentWithDots.Create [
                            "loadedAssembly"
                            "GetTypes"
                        ]

                    let ``GetTypes()`` =
                        SynExpr.CreateInstanceMethodCall(instanceAndMethod, [], SynExpr.CreateUnit)

                    let findTypeByModuleName =
                        let ``Seq.find`` predicate =
                            (SynExpr.CreateLongIdent
                             <| LongIdentWithDots.Create [
                                 "Seq"
                                 "find"
                             ])
                            <@> (SynExpr.CreateParen predicate)

                        let ``fun t -> t.FullName =`` moduleName =
                            let args =
                                SynSimplePats.SimplePats(
                                    [
                                        SynSimplePat.CreateTyped(
                                            Ident.Create "t",
                                            SynType.CreateLongIdent("System.Type")
                                        )
                                    ],
                                    range.Zero
                                )

                            let body =
                                SynExpr.CreateLongIdent(LongIdentWithDots.CreateString "t.FullName")
                                |> DSL.equalsSign (
                                    SynExpr.CreateConst
                                    <| SynConst.CreateString moduleName
                                )

                            SynExpr.CreateLambda(args, body, None)

                        ``Seq.find``(``fun t -> t.FullName =`` moduleName)

                    ``GetTypes()``
                    |> DSL.pipeRight findTypeByModuleName

                let letBinding =
                    { SynBindingRcd.Let with
                        Expr = rightHandSide
                        Pattern = leftHandSize
                        Access = Some SynAccess.Private
                    }

                SynModuleDecl.CreateLet [ letBinding ]

            let ``open`` ``namespace`` =
                DSL.openNamespace (DSL.createLongIdentWithDots ``namespace``)

            let createTypeOf (ty: Type) =
                let leftHandSide =
                    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString("loadedType"), [])


                let rightHandSide =
                    TypeHelpers.getFSTypeName ty
                    |> fst
                    |> DSL.createTypeOf

                { SynBindingRcd.Let with
                    Expr = rightHandSide
                    Pattern = leftHandSide
                    Access = Some SynAccess.Private
                }
                |> List.singleton
                |> SynModuleDecl.CreateLet

            let createRecordGetters (ty: Type) =
                let getters =
                    ty.GetProperties(
                        BindingFlags.Instance
                        ||| BindingFlags.NonPublic
                    )

                [
                    for getter in getters do


                        let leftHandSide =
                            let genericNames =
                                seq {
                                    yield
                                        getter.PropertyType
                                        |> TypeHelpers.getFSTypeName
                                }
                                |> Seq.collect (fun (_, gs) -> gs)
                                |> Seq.distinct
                                |> Seq.map (fun name ->
                                    SynTyparDecl.TyparDecl(
                                        SynAttributes.Empty,
                                        SynTypar.Typar(Ident.Create name, NoStaticReq, false)
                                    )
                                )
                                |> Seq.toList

                            let functionGenerics =
                                SynValTyparDecls.SynValTyparDecls(genericNames, false, [])

                            SynPat
                                .LongIdent(
                                    LongIdentWithDots.CreateString($"get_{getter.Name}"),
                                    None,
                                    Some functionGenerics,
                                    SynArgPats.Empty,
                                    None,
                                    range0
                                )
                                .ToRcd


                        let returnInfo =
                            let parameterSynTypesAndNamespaces = [
                                ty
                                |> TypeHelpers.getFSTypeName
                                getter.PropertyType
                                |> TypeHelpers.getFSTypeName
                            ]

                            let parameters =
                                parameterSynTypesAndNamespaces
                                |> List.map (fun (ts, _) -> ts)

                            let rec createReturnInfoType state list =
                                match state, list with
                                | None, head :: shoulders :: tail ->
                                    let newState = SynType.CreateFun(head, shoulders)
                                    createReturnInfoType (Some newState) tail
                                | Some (state), head :: tail ->
                                    let newState = SynType.CreateFun(state, head)
                                    createReturnInfoType (Some newState) tail
                                | None, [ f ] -> f
                                | None, [] -> SynType.CreateUnit
                                | Some state, [] -> state

                            (parameters
                             |> createReturnInfoType None)

                        let rightHandSide =
                            let funcExpr =
                                let getterFunc =
                                    "TheAngryByrd.TypeSafeInternals.Delegate.createGetter"

                                let generics =
                                    seq {
                                        ty
                                        |> TypeHelpers.getFSTypeName

                                        getter.PropertyType
                                        |> TypeHelpers.getFSTypeName
                                    }
                                    |> Seq.toList

                                let synExpr =
                                    SynExpr.CreateTypeApp
                                        (SynExpr.CreateLongIdent
                                         <| LongIdentWithDots.CreateString(getterFunc))
                                        (generics
                                         |> List.map (fun (ts, _) -> ts))

                                synExpr

                            let argsExpr =
                                SynExpr.CreateIdent
                                <| Ident.Create "loadedType"

                            let argsExpr2 =
                                SynExpr.CreateConst
                                <| SynConst.CreateString getter.Name

                            let curriedFunc =
                                funcExpr
                                <@> argsExpr
                                <@> argsExpr2

                            curriedFunc

                        let binding =
                            { SynBindingRcd.Let with
                                Pattern = leftHandSide
                                Expr = SynExpr.CreateTyped(rightHandSide, returnInfo)
                                ReturnInfo =
                                    Some
                                    <| SynBindingReturnInfoRcd.Create returnInfo
                            }

                        SynModuleDecl.CreateLet [ binding ]
                ]


            let createStaticMethod (mi: MethodInfo) =

                let miParams = mi.GetParameters()

                let getPrivateFuncName name = $"p_%s{name}"

                let getCachedFuncName name =
                    $"{getPrivateFuncName (name)}CachedFunc"

                let synModule =

                    let cachedCall =
                        let leftHandSide =
                            let genericNames =
                                seq {
                                    yield!
                                        miParams
                                        |> Array.map (fun p -> p.ParameterType)
                                        |> Array.map TypeHelpers.getFSTypeName

                                    yield
                                        mi.ReturnType
                                        |> TypeHelpers.getFSTypeName
                                }
                                |> Seq.collect (fun (_, gs) -> gs)
                                |> Seq.distinct
                                |> Seq.map (fun name ->
                                    SynTyparDecl.TyparDecl(
                                        SynAttributes.Empty,
                                        SynTypar.Typar(Ident.Create name, NoStaticReq, false)
                                    )
                                )
                                |> Seq.toList

                            let functionGenerics =
                                SynValTyparDecls.SynValTyparDecls(genericNames, false, [])

                            SynPat
                                .LongIdent(
                                    LongIdentWithDots.CreateString(getCachedFuncName mi.Name),
                                    None,
                                    Some functionGenerics,
                                    SynArgPats.Empty,
                                    Some SynAccess.Private,
                                    range0
                                )
                                .ToRcd


                        let returnInfo =
                            let parameterSynTypesAndNamespaces =
                                seq {
                                    if miParams.Length > 0 then
                                        yield!
                                            miParams
                                            |> Array.map (fun p -> p.ParameterType)
                                            |> Array.map TypeHelpers.getFSTypeName
                                    else
                                        yield (SynType.CreateUnit, [])

                                    yield
                                        mi.ReturnType
                                        |> TypeHelpers.getFSTypeName
                                }
                                |> Seq.toList

                            let parameters =
                                parameterSynTypesAndNamespaces
                                |> List.map (fun (ts, _) -> ts)

                            let rec createReturnInfoType state list =
                                match state, list with
                                | None, head :: shoulders :: tail ->
                                    let newState = SynType.CreateFun(head, shoulders)
                                    createReturnInfoType (Some newState) tail
                                | Some (state), head :: tail ->
                                    let newState = SynType.CreateFun(state, head)
                                    createReturnInfoType (Some newState) tail
                                | None, [ f ] -> f
                                | None, [] -> SynType.CreateUnit
                                | Some state, [] -> state

                            (parameters
                             |> createReturnInfoType None)

                        let rightHandSide =
                            let funcExpr =
                                let staticFunc arity =
                                    if mi.ReturnType = typeof<Void> then
                                        $"TheAngryByrd.TypeSafeInternals.Delegate.createStaticArity%d{arity}ReturningUnit"
                                    else
                                        $"TheAngryByrd.TypeSafeInternals.Delegate.createStaticArity%d{arity}"

                                let generics =
                                    seq {
                                        yield!
                                            miParams
                                            |> Array.map (fun p -> p.ParameterType)
                                            |> Array.map TypeHelpers.getFSTypeName

                                        if
                                            mi.ReturnType
                                            <> typeof<Void>
                                        then
                                            yield
                                                mi.ReturnType
                                                |> TypeHelpers.getFSTypeName
                                    }
                                    |> Seq.toList

                                let synExpr =
                                    SynExpr.CreateTypeApp
                                        (SynExpr.CreateLongIdent
                                         <| LongIdentWithDots.CreateString(
                                             staticFunc miParams.Length
                                         ))
                                        (generics
                                         |> List.map (fun (ts, _) -> ts))

                                synExpr

                            let argsExpr =
                                SynExpr.CreateIdent
                                <| Ident.Create "loadedModule"

                            let argsExpr2 =
                                SynExpr.CreateConst
                                <| SynConst.CreateString mi.Name

                            let curriedFunc =
                                funcExpr
                                <@> argsExpr
                                <@> argsExpr2

                            curriedFunc

                        let binding =
                            { SynBindingRcd.Let with
                                Pattern = leftHandSide
                                Expr = SynExpr.CreateTyped(rightHandSide, returnInfo)
                                ReturnInfo =
                                    Some
                                    <| SynBindingReturnInfoRcd.Create returnInfo
                            }

                        SynModuleDecl.CreateLet [ binding ]


                    let realCall =
                        let leftHandSide =
                            let args =
                                if miParams.Length = 0 then
                                    [ SynPatRcd.CreateUnit() ]
                                else
                                    miParams
                                    |> Array.map (fun p ->
                                        SynPatRcd.CreateNamed(
                                            Ident.Create p.Name,
                                            SynPatRcd.CreateWild
                                        )
                                    )
                                    |> Array.toList

                            SynPatRcd.CreateLongIdent(
                                LongIdentWithDots.CreateString($"{getPrivateFuncName (mi.Name)}"),
                                args
                            )

                        let rightHandSide =
                            let funcExpr =
                                let staticFunc = getCachedFuncName mi.Name

                                let synExpr =
                                    SynExpr.CreateTypeApp
                                        (SynExpr.CreateLongIdent
                                         <| LongIdentWithDots.CreateString(staticFunc))
                                        []

                                synExpr

                            let curriedFunc = funcExpr

                            let retFunc =
                                if miParams.Length = 0 then
                                    curriedFunc
                                    <@> SynExpr.CreateUnit
                                else
                                    (curriedFunc, miParams)
                                    ||> Array.fold (fun state p ->
                                        state
                                        <@> (SynExpr.CreateIdent
                                             <| Ident.Create p.Name)
                                    )

                            retFunc

                        let binding =
                            { SynBindingRcd.Let with
                                Pattern = leftHandSide
                                Expr = rightHandSide
                            }

                        SynModuleDecl.CreateLet [ binding ]


                    [
                        cachedCall
                        realCall
                    ]


                synModule

            let createModule
                assemblyName
                (ProCompiledModuleName pcModuleName)
                (moduleName: ReflectedModuleName)
                staticMethods
                visibleInternalRecords
                =
                let staticMethodDecls = [
                    yield ``let private loadedAssembly = Assembly.Load`` assemblyName
                    yield
                        ``let private sqlmodule = loadedAssembly.GetTypes() |> Seq.find(fun t -> t.FullName =``
                            moduleName
                    let ms =
                        staticMethods
                        |> Seq.map createStaticMethod
                        |> Seq.toList

                    for m in
                        ms
                        |> List.collect id do
                        yield m
                ]

                let visibleInternalRecordsDecls = [

                    for (rcd: Type) in visibleInternalRecords do
                        let moduleName = SynComponentInfoRcd.Create(Ident.CreateLong rcd.Name)

                        let moduleBody = [
                            createTypeOf rcd
                            yield! createRecordGetters rcd
                        // let typeName =
                        //     match rcd |> TypeHelpers.getFSTypeName |> fst with
                        //     | SynType.LongIdent li -> li
                        //     | _ -> failwith "No"
                        // let members = [
                        //     // SynMemberDefn.CreateMember <| SynBindingRcd
                        // ]
                        // SynModuleDecl.CreateType (SynComponentInfoRcd.Create typeName.Lid , members)
                        ]

                        SynModuleDecl.CreateNestedModule(moduleName, moduleBody)


                ]

                (Ident.CreateLong pcModuleName,
                 staticMethodDecls
                 @ visibleInternalRecordsDecls)

            let moduleTree =
                [
                    for generateMe in infos do
                        for moduleInfos in generateMe.ModuleInfos do
                            if
                                moduleInfos.StaticMethods
                                |> Seq.length > 0
                            then
                                if
                                    moduleInfos.Type
                                    |> FSharpType.IsExceptionRepresentation
                                    |> not
                                then // TODO: Filter out non-visible return types
                                    let reflectionName =
                                        ReflectedModuleName moduleInfos.Type.FullName

                                    let precompiledModuleNamed =
                                        ProCompiledModuleName(
                                            TypeHelpers.replaceModuleFullName moduleInfos.Type
                                        )

                                    let assemblyName = generateMe.Assembly.FullName

                                    yield
                                        createModule
                                            assemblyName
                                            precompiledModuleNamed
                                            reflectionName
                                            moduleInfos.StaticMethods
                                            moduleInfos.VisibleInternalRecords
                ]
                |> ModuleTree.fromExtractRecords

            let rec createModulesAndClasses (moduleTree) =
                ([], moduleTree)
                ||> List.fold (fun state x ->
                    state
                    @ match x with
                      | ModuleTree.Module (name, mods) -> [
                          let moduleId = SynComponentInfoRcd.Create(Ident.CreateLong name)
                          let decls = createModulesAndClasses mods
                          SynModuleDecl.CreateNestedModule(moduleId, decls)
                        ]
                      | ModuleTree.Leaf (decls) -> decls
                )


            let ``namespace`` = "TypeSafeInternals"

            [
                SynModuleOrNamespace.CreateNamespace(
                    Ident.CreateLong ``namespace``,
                    decls = createModulesAndClasses moduleTree
                )
            ]
            |> Output.Ast
