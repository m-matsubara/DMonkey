unit ecma_interface;

interface

type
  IJValue = interface
  end;

  IJValueList = interface
  end;

  IJNotify = interface
    procedure Notification(ANotify: IJNotify);
    procedure FreeNotification(ANotify: IJNotify);
    procedure RemoveFreeNotification(ANotify: IJNotify);
  end;

  IJObject = interface(IJNotify)
    procedure RegistMethods;
    procedure Clear;
    procedure RegistProperty(PropName: WideString; Value: IJValue);
    procedure RegistName(AName: WideString);
    function CallEvent(Prefix,EventName: WideString; Param: IJValueList;
      var RetVal: IJValue; RetParam: IJValueList = nil): WordBool;
    function IsCallEvent(EventName: WideString = ''): WordBool;

    function IncRef: Integer;
    function DecRef: Integer;
    function HasKey(S: WideString): WordBool;
    function RemoveKey(S: WideString): WordBool;
    function GetValue(S: WideString; ArrayStyle: WordBool; Param: IJValueList = nil): IJValue;
    procedure SetValue(S: WideString; Value: IJValue; ArrayStyle: WordBool; Param: IJValueList = nil);
    function ToString(Value: IJValue = nil): WideString;
    function ValueOf: IJValue;
    function ToNumber: Double;
    function ToBool: WordBool;
    function ToChar: Char;
    function Equal(Obj: IJObject): WordBool; 
    function GetPropertyList: WideString;
    function GetMethodList: WideString;
    function IsMakeGlobalInstance: WordBool;
    function GetName: String;

    function New: IJObject;
  end;

  IDMonkeyEvent = interface
    procedure OnNewObject;
    procedure OnStdout;
    procedure OnStderr;
    procedure OnRun;
    procedure OnDone;
    procedure OnStep;
    procedure OnStdin;
    procedure OnError;
    procedure OnDebugout;
    procedure OnDoEvents;
  end;

  IDMonkey = interface
    function Compile(SourceCode: WideString): WordBool;
    function CompileFile(AFilename: WideString): WordBool;
    function Run(Args: IJValueList): Integer;
    function CallFunction(Symbol: WideString; Param: IJValueList;
      var RetValue: IJValue): WordBool; 
    procedure Clear;
    procedure Abort;
    procedure ImportObject(ObjectName: WideString; MasterObject: IJObject);
    function IsRunning: WordBool;
    procedure AddEvent(AEvent: IDMonkeyEvent);
    procedure RemoveEvent(AEvent: IDMonkeyEvent);

    function ScriptBuild: Integer;
    function ScriptEngine: WideString;
    function ScriptVersion: WideString;

    function GetObjectCount: Integer;
    function GetTookTimeToCompile: Cardinal;
    function GetTookTimeToRun: Cardinal;
    function GetTookTimeToCallFunction: Cardinal;
    function GetScriptFilename: WideString;
    function GetLineNumber: Integer;
    function GetDeclareLocalVar: WordBool;
    procedure SetDeclareLocalVar(Value: WordBool);
    function GetLibraryPath: WideString;
    procedure SetLibraryPath(Value: WideString);
    function GetCompiledBinary: WordBool;
    procedure SetCompiledBinary(Value: WordBool);
  end;


implementation

uses
  dmonkey,ecma_type;

end.
