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
type FormResetProp =
    static member inline values (value: 'FormValues) = rhfFormResetProp<'FormValues> "values" value
    static member inline keepErrors (value: bool) = rhfFormResetProp "keepErrors" value
    static member inline keepDirty (value: bool) = rhfFormResetProp "keepDirty" value
    static member inline keepDirtyValues (value: bool) = rhfFormResetProp "keepDirtyValues" value
    static member inline keepValues (value: bool) = rhfFormResetProp "keepValues" value
    static member inline keepDefaultValues (value: bool) = rhfFormResetProp "keepDefaultValues" value
    static member inline keepIsSubmitted (value: bool) = rhfFormResetProp "keepIsSubmitted" value
    static member inline keepTouched (value: bool) = rhfFormResetProp "keepTouched" value
    static member inline keepIsValid (value: bool) = rhfFormResetProp "keepIsValid" value
    static member inline keepSubmitCount (value: bool) = rhfFormResetProp "keepSubmitCount" value


[<Erase>]
type RhfRule =
    static member inline required (value: bool) = rhfRule "required" value
    static member inline required (value: ValidationRule<bool>) = rhfRule "required" value
    static member inline min (value: float) = rhfRule "min" value
    static member inline min (value: ValidationRule<float>) = rhfRule "min" value
    static member inline min (value: string) = rhfRule "min" value
    static member inline min (value: ValidationRule<string>) = rhfRule "min" value
    static member inline max (value: float) = rhfRule "max"  value
    static member inline max (value: ValidationRule<float>) = rhfRule "max"  value
    static member inline max (value: string) = rhfRule "max"  value
    static member inline max (value: ValidationRule<string>) = rhfRule "max"  value
    static member inline maxLength (value: float) = rhfRule "maxLength" value
    static member inline maxLength (value: ValidationRule<float>) = rhfRule "maxLength" value
    static member inline minLength (value: float) = rhfRule "minLength" value
    static member inline minLength (value: ValidationRule<float>) = rhfRule "minLength" value
    static member inline validate (value: RuleValidateOption<'T>) = rhfRule "validate" value
    static member inline validate (value: RuleValidatePromiseOption<'T>) = rhfRule "validate" value
    static member inline validate (value: RuleValidateResult<'T>) =
        value >> mapResultToOption |> RhfRule.validate
    static member inline validate (value: RuleValidateAsyncResult<'T>) =
        value >> mapAsyncResultToOption |> RhfRule.validate
    static member inline pattern (regex: System.Text.RegularExpressions.Regex, message: string) =
        rhfRule "pattern" (ValidationRule.create regex message)
    static member inline pattern (regexPattern: string, message: string) =
        RhfRule.pattern (Regex regexPattern, message)


[<Erase>]
module RhfForm =
    [<Erase>]
    type mode =
        static member inline onChange = rhfFormProp "mode" "onChange"
        static member inline onBlur = rhfFormProp "mode" "onBlur"
        static member inline onSubmit = rhfFormProp "mode" "onSubmit"
        static member inline onTouched = rhfFormProp "mode" "onTouched"
        static member inline all = rhfFormProp "mode" "all"

[<Erase>]
type RhfForm =
    static member inline defaultValues (value: 'FormValues) = rhfFormProp "defaultValues" value

[<Erase>]
type RhfController =
    static member inline name (value: string) = rhfControllerProp "name" value
    static member inline name ([<InlineIfLambda>] getField: 'FormValues -> 'Value) =
        let fieldName = Experimental.namesofLambda getField |> String.concat "."
        RhfController.name fieldName : IRhfControllerProp<'FormValues, 'Value>
    static member inline rules (rules: #seq<IRhfRule>) = rhfControllerProp "rules" (createObj !!rules)
    static member inline defaultValue (value: 'Value) = rhfControllerProp<_, 'Value> "defaultValue" value
    static member inline control (value: Control<'FormValues>) = rhfControllerProp<'FormValues, _> "control" value


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
