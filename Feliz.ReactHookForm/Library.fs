module [<AutoOpen>] Feliz.ReactHookForm.ReactHookForm

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Fable.ReactHookForm

type Regex = System.Text.RegularExpressions.Regex

let inline FormProvider (props: UseFormReturn<'T>) children : ReactElement =
    Fable.React.Helpers.ofImport "FormProvider" "react-hook-form" props children


let inline useFormContext<'T> () : UseFormReturn<'T> =
    (import "useFormContext" "react-hook-form") ()


let inline DevTool (control: Control<'T>) =
    (import "DevTool" "@hookform/devtools") {| control = control |}

type ValidationRule<'TValidationValue> = {|
    value: 'TValidationValue;
    message: string
|}

module ValidationRule =
    let inline create value message : ValidationRule<'ValidationValue> =
        {| value = value; message = message |}

type IRhfFormProp = interface end
type IRhfControllerProp = interface end
type IRhfRule = interface end
type IRhfFormResetProp = interface end

let inline rhfFormProp (name: string) (value: 'T) : IRhfFormProp = unbox (name, value)
let inline rhfControllerProp (name: string) (value: 'T) : IRhfControllerProp = unbox (name, value)
let inline rhfRule (name: string) (value: 'T) : IRhfRule = unbox (name, value)
let inline rhfFormResetProp (name: string) (value: 'T) : IRhfFormResetProp = unbox (name, value)

type private SubmitHandlerPromise<'T> = 'T -> JS.Promise<unit>
type private SubmitErrorHandlerPromise = obj -> JS.Promise<unit>


type private ControllerRenderPropsInternal<'F> =
    abstract name: string
    abstract value: 'F
    abstract onChange: (Browser.Types.Event -> 'F -> unit)
    abstract onBlur: (unit -> unit)

type private ControllerFieldStateInternal =
    abstract invalid: bool
    abstract isTouched: bool
    abstract isDirty: bool
    abstract error: ControllerValidationError option

type private UseFormReturnInternal<'T> =
    abstract control: Control<'T>
    abstract handleSubmit: SubmitHandlerPromise<'T> -> SubmitErrorHandlerPromise -> Func<Browser.Types.Event, unit>
    abstract reset: obj -> obj -> unit
    abstract formState: FormState<'T>
    abstract getValues: obj

type private UseControllerReturnInternal<'F> =
    abstract field: ControllerRenderPropsInternal<'F>
    abstract fieldState: ControllerFieldStateInternal


type ControllerRenderProps<'F> = {
    name: string
    value: 'F
    onChange: ('F -> unit)
    onChangeEvent: Browser.Types.Event -> 'F -> unit
    onBlur: unit -> unit
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
type FormResetProp<'T> =
    static member inline values (value: 'T) = rhfFormResetProp "values" value
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
        let name = Experimental.namesofLambda getField |> String.concat "."
        rhfControllerProp "name" name
    static member inline rules (rules: #seq<IRhfRule>) = rhfControllerProp "rules" (createObj !!rules)
    static member inline defaultValue (value: 'T) = rhfControllerProp "defaultValue" value
    static member inline control (value: Control<'T>) = rhfControllerProp "control" value



[<Hook>]
let useForm<'FormValues> (props: seq<IRhfFormProp>) : UseFormReturn<'FormValues> =
    let r: UseFormReturnInternal<'FormValues> =
        (import "useForm" "react-hook-form") (createObj !!props)

    let handleSubmit = React.useCallback (fun (s: SubmitHandler<'FormValues>) (e: SubmitErrorHandler) ->
        (r.handleSubmit !!s !!e).Invoke
    , [| r.handleSubmit |])

    let handleSubmitAsync = React.useCallback(fun s e ->
        let sp = s >> Async.StartAsPromise
        let ep = e >> Async.StartAsPromise
        (r.handleSubmit sp ep).Invoke
    , [| r.handleSubmit |])

    let reset = React.useCallback(fun (opts: FormResetProps<'FormValues> list) ->
        let (v, o) =
            opts |> List.partition (function
                | Values _ -> true
                | _ -> false)

        let newValue =
            List.tryHead v
            |> Option.map (function
                | Values v -> Some v
                | _ -> None)
            |> Option.flatten

        keyValueList CaseRules.LowerFirst o
        |> r.reset newValue
    , [| r.reset |])

    React.useMemo(fun () ->
        {
            control = r.control
            handleSubmit = handleSubmit
            handleSubmitAsync = handleSubmitAsync
            reset = reset
            formState = r.formState
            getValues = r.getValues
        }
    , [| r.control; handleSubmit; handleSubmitAsync; reset; r.formState; r.getValues |])


[<Hook>]
let useController<'FormValues, 'Value> (props: seq<IRhfControllerProp>) : UseControllerReturn<'FormValues, 'Value> =
    let r: UseControllerReturnInternal<'Value> =
        (import "useController" "react-hook-form") (createObj !!props)

    let error = React.useMemo(fun () ->
        r.fieldState.error
        |> Option.defaultValue { message = "" }
    , [| r.fieldState.error |])

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
                onChange = !!r.field.onChange
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
            onChange = !!r.field.onChange
            onChangeEvent = r.field.onChange
            onBlur = r.field.onBlur
        }
    , [|
        r.fieldState.invalid; r.fieldState.isTouched; r.fieldState.isDirty
        error; r.field.name; r.field.value; r.field.onChange; r.field.onBlur
        error.message
    |])
