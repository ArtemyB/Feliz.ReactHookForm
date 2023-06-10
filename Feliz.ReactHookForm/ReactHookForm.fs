module [<AutoOpen>] Feliz.ReactHookForm.ReactHookForm

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Bindings

type SubmitHandler<'T> = 'T -> unit
type SubmitErrorHandler = obj -> unit
type SubmitHandlerAsync<'T> = 'T -> Async<unit>
type SubmitErrorHandlerAsync = obj -> Async<unit>


type ControllerRenderProps<'F> = {
    name: string
    value: 'F
    onChange: ('F -> unit)
    onChangeEvent: Browser.Types.Event -> 'F -> unit
    onBlur: unit -> unit
}

type ControllerFieldState = {
    invalid: bool
    isTouched: bool
    isDirty: bool
    error: ControllerValidationError
}

type UseControllerReturn<'T,'F> = {
    field: ControllerRenderProps<'F>
    fieldState: ControllerFieldState
    name: string
    value: 'F
    onChange: 'F -> unit
    onBlur: unit -> unit
    onChangeEvent: Browser.Types.Event -> 'F -> unit
    invalid: bool
    isTouched: bool
    isDirty: bool
    error: ControllerValidationError
    errorMessage: string
}

type UseFormReturn<'T> = {
    control: Control<'T>
    handleSubmit: SubmitHandler<'T> -> SubmitErrorHandler -> Browser.Types.Event -> unit
    handleSubmitAsync: SubmitHandlerAsync<'T> -> SubmitErrorHandlerAsync -> Browser.Types.Event -> unit
    reset: IRhfFormResetProp<'T> list -> unit
    formState: FormState<'T>
    getValues: obj
}


let inline private mapResultToOption (result: Result<'T, string>) : string option =
    match result with
    | Ok _ -> None
    | Error msg -> Some msg

let inline private mapAsyncResultToOption (result: Async<Result<'T, string>>) : JS.Promise<string option> =
    async {
        let! result' = result
        return (mapResultToOption result')
    }
    |> Async.StartAsPromise


[<Erase>]
type rhfFormResetProp =
    static member inline values (value: 'FormValues) = Interop.rhfFormResetProp<'FormValues> "values" value
    static member inline keepErrors (value: bool) = Interop.rhfFormResetProp "keepErrors" value
    static member inline keepDirty (value: bool) = Interop.rhfFormResetProp "keepDirty" value
    static member inline keepDirtyValues (value: bool) = Interop.rhfFormResetProp "keepDirtyValues" value
    static member inline keepValues (value: bool) = Interop.rhfFormResetProp "keepValues" value
    static member inline keepDefaultValues (value: bool) = Interop.rhfFormResetProp "keepDefaultValues" value
    static member inline keepIsSubmitted (value: bool) = Interop.rhfFormResetProp "keepIsSubmitted" value
    static member inline keepTouched (value: bool) = Interop.rhfFormResetProp "keepTouched" value
    static member inline keepIsValid (value: bool) = Interop.rhfFormResetProp "keepIsValid" value
    static member inline keepSubmitCount (value: bool) = Interop.rhfFormResetProp "keepSubmitCount" value


[<Erase>]
type rhfRule =
    static member inline required (value: bool) = Interop.rhfRule "required" value
    static member inline required (value: ValidationRule<bool>) = Interop.rhfRule "required" value
    static member inline min (value: float) = Interop.rhfRule "min" value
    static member inline min (value: ValidationRule<float>) = Interop.rhfRule "min" value
    static member inline min (value: string) = Interop.rhfRule "min" value
    static member inline min (value: ValidationRule<string>) = Interop.rhfRule "min" value
    static member inline max (value: float) = Interop.rhfRule "max"  value
    static member inline max (value: ValidationRule<float>) = Interop.rhfRule "max"  value
    static member inline max (value: string) = Interop.rhfRule "max"  value
    static member inline max (value: ValidationRule<string>) = Interop.rhfRule "max"  value
    static member inline maxLength (value: float) = Interop.rhfRule "maxLength" value
    static member inline maxLength (value: ValidationRule<float>) = Interop.rhfRule "maxLength" value
    static member inline minLength (value: float) = Interop.rhfRule "minLength" value
    static member inline minLength (value: ValidationRule<float>) = Interop.rhfRule "minLength" value
    static member inline validate (value: RuleValidateOption<'T>) = Interop.rhfRule "validate" value
    static member inline validate (value: RuleValidatePromiseOption<'T>) = Interop.rhfRule "validate" value
    static member inline validate (value: RuleValidateResult<'T>) =
        value >> mapResultToOption |> rhfRule.validate
    static member inline validate (value: RuleValidateAsyncResult<'T>) =
        value >> mapAsyncResultToOption |> rhfRule.validate
    static member inline pattern (regex: System.Text.RegularExpressions.Regex, message: string) =
        Interop.rhfRule "pattern" (ValidationRule.create regex message)
    static member inline pattern (regexPattern: string, message: string) =
        rhfRule.pattern (Regex regexPattern, message)


[<Erase>]
module rhfForm =
    [<Erase>]
    type mode =
        static member inline onChange = Interop.rhfFormProp "mode" "onChange"
        static member inline onBlur = Interop.rhfFormProp "mode" "onBlur"
        static member inline onSubmit = Interop.rhfFormProp "mode" "onSubmit"
        static member inline onTouched = Interop.rhfFormProp "mode" "onTouched"
        static member inline all = Interop.rhfFormProp "mode" "all"

[<Erase>]
type rhfForm =
    static member inline defaultValues (value: 'FormValues) = Interop.rhfFormProp "defaultValues" value

[<Erase>]
type rhfController =
    static member inline name (value: string) = Interop.rhfControllerProp "name" value
    static member inline name ([<InlineIfLambda>] getField: 'FormValues -> 'Value) =
        let fieldName = Experimental.namesofLambda getField |> String.concat "."
        rhfController.name fieldName : IRhfControllerProp<'FormValues, 'Value>
    static member inline rules (rules: #seq<IRhfRule>) = Interop.rhfControllerProp "rules" (createObj !!rules)
    static member inline defaultValue (value: 'Value) = Interop.rhfControllerProp<_, 'Value> "defaultValue" value
    static member inline control (value: Control<'FormValues>) = Interop.rhfControllerProp<'FormValues, _> "control" value


let inline DevTool (control: Control<'T>) =
    (import "DevTool" "@hookform/devtools") {| control = control |}


[<Hook>]
let internal useHandleSubmit (r: UseFormReturnInternal<'T>) =
    React.useCallback (fun (s: SubmitHandler<'FormValues>) (e: SubmitErrorHandler) ->
        (r.handleSubmit !!s !!e).Invoke
    , [| r.handleSubmit |])

[<Hook>]
let internal useHandleSubmitAsync (r: UseFormReturnInternal<'T>) =
    React.useCallback(fun s e ->
        let sp = s >> Async.StartAsPromise
        let ep = e >> Async.StartAsPromise
        (r.handleSubmit sp ep).Invoke
    , [| r.handleSubmit |])

[<Hook>]
let internal useReset (r: UseFormReturnInternal<'T>) =
    React.useCallback(fun (opts: IRhfFormResetProp<'FormValues> list) ->
        let (v, o) =
            opts |> List.partition (fun prop ->
                match unbox<string * obj> prop with
                | ("values", _) -> true
                | _ -> false)

        let newValue =
            List.tryHead v
            |> Option.map (fun prop ->
                match unbox<string * obj> prop with
                | ("values", v) -> Some v
                | _ -> None)
            |> Option.flatten

        createObj !!o |> r.reset newValue
    , [| r.reset |])


[<Hook>]
let internal useFormReturn<'T> (form: UseFormReturnInternal<'T>) : UseFormReturn<'T> =
    let handleSubmit = useHandleSubmit form
    let handleSubmitAsync = useHandleSubmitAsync form
    let reset = useReset form

    React.useMemo(fun () ->
        {
            control = form.control
            handleSubmit = handleSubmit
            handleSubmitAsync = handleSubmitAsync
            reset = reset
            formState = form.formState
            getValues = form.getValues
        }
    , [| form.control; handleSubmit; handleSubmitAsync; reset; form.formState; form.getValues |])


let inline FormProvider (props: UseFormReturn<'T>) children : ReactElement =
    Fable.React.Helpers.ofImport "FormProvider" "react-hook-form" props children

[<Hook>]
let useFormContext<'T> () : UseFormReturn<'T> =
    let form = Bindings.useFormContext<'T> ()
    useFormReturn form


[<Hook>]
let useForm<'FormValues> (props: seq<IRhfFormProp>) : UseFormReturn<'FormValues> =
    let r = Bindings.useForm<'FormValues> props
    useFormReturn r


[<Hook>]
let useController<'FormValues, 'Value> (props: seq<IRhfControllerProp<'FormValues, 'Value>>) : UseControllerReturn<'FormValues, 'Value> =
    let r = Bindings.useController<'FormValues, 'Value> props

    let error = React.useMemo(fun () ->
        r.fieldState.error
        |> Option.defaultValue !!{| message = "" |}
    , [| r.fieldState.error |])

    let onChange = React.useCallback(r.field.onChange null, [| r.field.onChange |])

    React.useMemo(fun () ->
        {
            fieldState = {
                invalid = r.fieldState.invalid
                isTouched = r.fieldState.isTouched
                isDirty = r.fieldState.isDirty
                error = error
            }
            field = {
                name = r.field.name
                value = r.field.value
                onChange = onChange
                onChangeEvent = r.field.onChange
                onBlur = r.field.onBlur
            }
            invalid = r.fieldState.invalid
            isTouched = r.fieldState.isTouched
            isDirty = r.fieldState.isDirty
            error = error
            errorMessage = error.message
            name = r.field.name
            value = r.field.value
            onChange = onChange
            onChangeEvent = r.field.onChange
            onBlur = r.field.onBlur
        }
    , [|
        r.fieldState.invalid; r.fieldState.isTouched; r.fieldState.isDirty
        error; r.field.name; r.field.value; r.field.onChange; r.field.onBlur
        error.message
    |])
