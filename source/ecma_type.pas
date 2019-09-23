unit ecma_type;

//型を定義
//2001/04/10 ~
//by Wolfy


{$IFDEF VER130}
  {$DEFINE DELPHI4_5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE DELPHI4_5}
{$ENDIF}

{$IFDEF CONSOLE}
  {$DEFINE NO_VCL}
  {$DEFINE NO_GUI}
{$ENDIF}

interface

uses
  windows,sysutils,classes,hashtable,typinfo,
  myclasses,contnrs,dynamiccall,ecma_interface
{$IFNDEF NO_ACTIVEX}
  ,activex
{$ENDIF}
{$IFDEF DELPHI4_5}
  ;
{$ELSE}
  ,variants;
{$ENDIF}

const //バージョンを定義する
  DMS_ENGINE = 'DMScript';
  DMS_BUILD = 42;
  DMS_MAJOR = 0;
  DMS_MINOR = 3.9;
  DMS_VERSION = '0.3.9';
  //シリアライズver
  DMS_SERIALIZE_VER: Byte = 18;
  //拡張子定義
  DMS_EXT = '.dms';
  DMS_COMPILED_EXT = '.dmc';

  CR = #13;
  LF = #10;
  CRLF = #13#10;

  //条件コンパイル
  CC_VERSION_7 = 'VERSION7';
  CC_SHORT_CIRCUIT = 'SHORT_CIRCUIT';

  __MAIN__ = '__MAIN__';

{$IFDEF DELPHI4_5}
type
  PPWideChar = ^PWideChar;
  PPChar     = ^PChar;
{$ENDIF}

type
  PJStatement = ^TJStatement;
  PJValue = ^TJValue;
  PJExpr = ^TJExpr;
  IJFunction = interface;
  TJObject = class;
  TJHash = class;
  TJValueList = class;
  TJObjectFactory = class;
  PJObjectClass = ^TJObjectClass;
  TJObjectClass = class of TJObject;
  TJBaseEngine = class;
  TJBaseDMonkey = class(TComponent);
  TJRootSymbolTable = class;
  TJGlobalSymbolTable = class;
  TJFunctionSymbolTable = class;
  TJLocalSymbolTable = class;
  TJNotify = class;

  //イベント
  TStringEvent = procedure(Sender: TObject; S: String) of object;
  TRefStringEvent = procedure(Sender: TObject; var S: String) of object;
  TReadStringEvent = procedure(Sender: TObject; var S: String; var Success: Boolean;
    Count: Integer; Line: Boolean) of object;
  TStepEvent = procedure(Sender: TObject; var Abort: Boolean) of object;
  TNewObjectEvent = procedure(Sender: TObject; JObject: TJObject) of object;
  TErrorEvent = procedure(Sender: TObject; LineNo: Integer; Msg: String) of object;

  //値の型
  TJValueType = (vtUndefined,vtNull,vtInteger,vtDouble,
                 vtString,vtObject,vtBool,vtFunction,vtInfinity,vtNaN,
                 vtDispatch,vtRegExp,vtEvent);
  TJValueTypeSet = set of TJValueType;

  TJValueAttribute = (vaReadOnly,vaDontEnum,vaDontDelete,
                      vaInternal,vaPrivate,vaReference);
  TJValueAttributes = set of TJValueAttribute;

  //値を保存するレコード
  TJValue = record
    ValueType: TJValueType;       //値の種類
    //Attributes: TJValueAttributes; //値の属性

    vString: String;              //以下値の中身
    vDispatch: IDispatch;
    vFunction: IJFunction;
    case Integer of
      0: (vInteger: Integer);
      1: (vDouble: Double);
      2: (vBool: Boolean);
      3: (vNull: Pointer);
      4: (vObject: TJObject);
      6: (vRegExpOptions: String[7]); //igm
      7: (vEvent: TMethod);
  end;

  //関数型
  TJFuncType = (ftStatement,
                ftMethod,
                ftActiveX,
                ftClass,ftImport,
                ftDynaCall);

  TJMethod = function (Param: TJValueList): TJValue of object;
  TJActivexMethodFlag = (axfMethod,axfGet,axfPut);

  PJActiveXMethod = ^TJActiveXMethod;
  TJActiveXMethod = record
    Parent: IDispatch;
    Dispid: Integer;
    Flag: TJActivexMethodFlag;
  end;

  TJFunctionCallFlag = (fcfNone,fcfApply,fcfCall);

  __TJFunction = record
    Symbol: String;
    FuncType: TJFuncType;
    Parameter: PJStatement;
    Flag: TJFunctionCallFlag;
    FunctionTable: TJFunctionSymbolTable;

    vActiveX: TJActiveXMethod;
    vDynaCall: TDynaDeclare;
    case Integer of
      0: (vStatement: PJStatement);
      1: (vMethod: TJMethod);
  end;

  TVarRecArray = array of TVarRec;

  //アクション
  TJOPCode = (opNone,
              opExpr,
              opAdd,opSub,opDiv,opMul,opMod,opDivInt,
              opAssign,
              opMulAssign,opDivAssign,opAddAssign,opSubAssign,opModAssign,
              opBitLeftAssign,opBitRightAssign,opBitRightZeroAssign,
              opBitAndAssign,opBitXorAssign,opBitOrAssign,
              opConstant,opVariable,
              opPlus,opMinus,
              opThis,opMember,opObjectElement,opSuper,
              opNew,opNewObject,opNewArray,
              opCallArray,opArg,
              opPreInc,opPreDec,opPostInc,opPostDec,
              opDelete,opVoid,opTypeof,
              opLogicalNot,opLogicalOr,opLogicalOr2,opLogicalAnd,opLogicalAnd2,
              opBitLeft,opBitRight,opBitRightZero,
              opLS,opGT,opLSEQ,opGTEQ,
              opEQ,opNE,opEQEQEQ,opNEEQEQ,
              opBitAnd,opBitXor,opBitOr,opBitNot,
              opConditional,
              opFunction,opMethod,
              opVar);

  TJEvalExprFlag = (eefDelete,eefVar);
  TJEvalExprFlags = set of TJEvalExprFlag;

  //解析木
  TJExpr = record
    Code: TJOPCode;            //動作を示す
    Left,                       //左辺
    Right: PJExpr;              //右辺
    Third: PJExpr;              //３項
    Value: PJValue;             //定数値
    Symbol: String;             //変数名
    Statement: PJStatement;
  end;

  TJRegistVarType = (rvGlobal,rvLocal,rvStatic);

  TJStatementType = (stNone,stSource,
                     stBlock,
                     stExpr,
                     stIf,stWhile,stDo,
                     stFor,stForIn,stForInArrayElement,
                     stFunctionDecl,stParamDecl,stClassDecl,stVariableDecl,
                     stBreak,stContinue,stReturn,
                     stTry,stCatch,stFinally,stThrow,
                     stWith,
                     stVar,
                     stLabeled,stSwitch,
                     stStatic,stGlobal);

  TJEvalStatementFlag = (esfVar,esfIteration,esfStaticVar,esfGlobalVar);
  TJEvalStatementFlags = set of TJEvalStatementFlag;

  //文 linked list
  TJStatement = record
    SType: TJStatementType;
    Expr: PJExpr;
    Prev: PJStatement;
    Next: PJStatement;
    Sub1,Sub2: PJStatement;
    Temp: PJStatement;
    LineNo: Integer;
  end;

  //switch用
  PJSwitchValue = ^TJSwitchValue;
  TJSwitchValue = record
    Value: PJValue;
    Default: PJStatement;
    Match: Boolean;
  end;

  IJFunction = interface
    function GetFunc: __TJFunction;
    function GetFunctionTable: TJFunctionSymbolTable;
    function GetLocalTable: TJLocalSymbolTable;
    function GetFlag: TJFunctionCallFlag;
    function GetFuncType: TJFuncType;
    function GetParameter: PJStatement;
    function GetSymbol: String;
    function GetvActiveX: PJActiveXMethod;
    function GetvDynaCall: PDynaDeclare;
    function GetvMethod: TJMethod;
    function GetvStatement: PJStatement;
    procedure SetFunctionTable(const Value: TJFunctionSymbolTable);
    procedure SetLocalTable(const Value: TJLocalSymbolTable);
    procedure SetFlag(const Value: TJFunctionCallFlag);
    procedure SetFuncType(const Value: TJFuncType);
    procedure SetParameter(const Value: PJStatement);
    procedure SetSymbol(const Value: String);
    procedure SetvMethod(const Value: TJMethod);
    procedure SetvStatement(const Value: PJStatement);
    function GetMethodOwner: TJObject;
    procedure SetMethodOwner(const Value: TJObject);

    procedure Assign(Source: IJFunction);

    property Symbol: String read GetSymbol write SetSymbol;
    property FuncType: TJFuncType read GetFuncType write SetFuncType;
    property Parameter: PJStatement read GetParameter write SetParameter;
    property Flag: TJFunctionCallFlag read GetFlag write SetFlag;
    property FunctionTable: TJFunctionSymbolTable read GetFunctionTable write SetFunctionTable;
    property LocalTable: TJLocalSymbolTable read GetLocalTable write SetLocalTable;
    property MethodOwner: TJObject read GetMethodOwner write SetMethodOwner;

    property vActiveX: PJActiveXMethod read GetvActiveX;
    property vDynaCall: PDynaDeclare read GetvDynaCall;
    property vStatement: PJStatement read GetvStatement write SetvStatement;
    property vMethod: TJMethod read GetvMethod write SetvMethod;
  end;

  TJFunctionImpl = class(TInterfacedObject,IJFunction)
  private
    FFunc: __TJFunction;
    FLocalTable: TJLocalSymbolTable;
    FMethodOwner: TJObject;
    FNotify: TJNotify;
    procedure NotifyOnNotifycation(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: IJFunction);

    function GetMethodOwner: TJObject;
    procedure SetMethodOwner(const Value: TJObject);
    function GetFunc: __TJFunction;
    function GetFunctionTable: TJFunctionSymbolTable;
    function GetLocalTable: TJLocalSymbolTable;
    function GetFlag: TJFunctionCallFlag;
    function GetFuncType: TJFuncType;
    function GetParameter: PJStatement;
    function GetSymbol: String;
    function GetvActiveX: PJActiveXMethod;
    function GetvDynaCall: PDynaDeclare;
    function GetvMethod: TJMethod;
    function GetvStatement: PJStatement;
    procedure SetFunctionTable(const Value: TJFunctionSymbolTable);
    procedure SetLocalTable(const Value: TJLocalSymbolTable);
    procedure SetFlag(const Value: TJFunctionCallFlag);
    procedure SetFuncType(const Value: TJFuncType);
    procedure SetParameter(const Value: PJStatement);
    procedure SetSymbol(const Value: String);
    procedure SetvMethod(const Value: TJMethod);
    procedure SetvStatement(const Value: PJStatement);
  end;

  { TODO : interfaceにする？ }
  //
  TJNotify = class(TPersistent)
  private
    FFreeNotifies: TBinList;
    FOnNotification: TNotifyEvent;
  protected
    procedure Notification(AObject: TJNotify); virtual;
  public
    destructor Destroy; override;
    procedure FreeNotification(AObject: TJNotify);
    procedure RemoveFreeNotification(AObject: TJNotify);

    property OnNotification: TNotifyEvent read FOnNotification write FOnNotification;
  end;


  //hash object
  TJHash = class(TCustomHashTable)
  private
    FNotify: TJNotify;
    procedure HashOnItemDispose(Sender: TObject; P: PHashItem);
    procedure NotifyOnNotifycation(Sender: TObject);
  public
    constructor Create(ATableSize: DWord; AIgnoreCase: Boolean = False); override;
    destructor Destroy; override;
    function GetValue(Key: String; var Value: TJValue): Boolean;
    procedure SetValue(Key: String; Value: TJValue);
    procedure ClearValue(Target,Ignore: TJValueTypeSet);
    procedure GetKeyList(List: TStrings; Need,Ignore: TJValueTypeSet);

    property Value[Key: String]: TJValue write SetValue;
  end;

  TJValueList = class(TObject)
  private
    FItems: TListPlus;
    FNotify: TJNotify;
    function GetItems(Index: Integer): TJValue;
    procedure SetItems(Index: Integer; const Value: TJValue);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetSortType: TSortType;
    procedure SetSortType(const Value: TSortType);

    procedure NotifyOnNotifycation(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Add(Value: TJValue; IncRef: Boolean = True): Integer; overload;
    function Add(Value: TJObject; IncRef: Boolean = True): Integer; overload;
    function Add(Value: Integer): Integer; overload;
    function Add(Value: Boolean): Integer; overload;
    function Add(Value: Double): Integer; overload;
    function Add(Value: String): Integer; overload;
    function Add(Value: IDispatch): Integer; overload;
    procedure Insert(Index: Integer; Value: TJValue);
    procedure Sort(Compare: TListSortCompareObj);
    procedure Assign(Source: TJValueList);

    property Items[Index: Integer]: TJValue read GetItems write SetItems; default;
    property Count: Integer read GetCount write SetCount;
    property SortType: TSortType read GetSortType write SetSortType;
  end;

  //基本オブジェクト
  //TJObject.Freeは絶対に呼ばないでください
  //
  //Objectを使用する場合はIncRef
  //Objectを解放したいときはDecRefを使います
  //
  //TJObjectを安全に所有できるコンテナはTJHash,TJValueListのみです
  //
  //TJMethod型をクラス内部で使った後、
  //返値がObjectならばDecRefしてください
  //TJMethodはpublicにしないことを推奨します
  TJObject = class(TJNotify)
  private
    FRefCount: Integer;
    FName: String;
    FMembers: TJHash;
    FEvents: TStringList;

  protected
    FEngine: TJBaseEngine;
    FDefaultProperties: TStringList;

    procedure RegistMethod(MethodName: String; Method: TJMethod);
    //メソッドを登録する
    //できればここでRegistMethodを使って欲しいが現在は必須ではない
    procedure RegistMethods; virtual;
    procedure Registproperties; virtual;

    procedure RegistEventName(EventName: String);

    procedure GetKeyList(List: TStringList; Need,Ignore: TJValueTypeSet);
    function HasDefaultProperty(Prop: String): Boolean;

    procedure ClearMembers;
    procedure ClearProperties;
    procedure ClearValue(Target,Ignore: TJValueTypeSet);

    function DoHasKey(Param: TJValueList): TJValue;
    function DoRemoveKey(Param: TJValueList): TJValue;
    function DoToString(Param: TJValueList): TJValue;
    function DoValueOf(Param: TJValueList): TJValue;
    function DoGetKeys(Param: TJValueList): TJValue;
    function DoGetProperties(Param: TJValueList): TJValue;
    function DoGetMethods(Param: TJValueList): TJValue;
    function DoGetEvents(Param: TJValueList): TJValue;

    function GetValueImpl(S: String; var RetVal: TJValue; Param: TJValueList = nil): Boolean; virtual;
    function SetValueImpl(S: String; var Value: TJValue; Param: TJValueList = nil): Boolean; virtual;
    //所有objectの終了通知
    procedure Notification(AObject: TJNotify); override;
  public
    //RegisteringFactory = trueで自身をFactoryに登録します(通常)
    //メンバにobjectを所有する場合にのみ、所有objectをfalseにして作成します(_test.pas参照)
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Clear; virtual;

    procedure RegistProperty(PropName: String; Value: TJValue);
    procedure RegistName(AName: String);
    function CallEvent(Prefix,EventName: String; Param: TJValueList): TJValue;
    function IsCallEvent(EventName: String = ''): Boolean;

    function IncRef: Integer; virtual;
    function DecRef: Integer; virtual;
    function HasKey(S: String): Boolean; virtual;
    function RemoveKey(S: String): Boolean; virtual;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; virtual;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); virtual;
    function ToString(Value: PJValue = nil): String; virtual;
    function ValueOf: TJValue; virtual;
    function ToNumber: Double; virtual;
    function ToBool: Boolean; virtual;
    function ToChar: Char; virtual;
    function Equal(Obj: TJObject): Boolean; virtual;
    procedure GetPropertyList(List: TStringList); virtual;
    procedure GetMethodList(List: TStringList); virtual;
    procedure GetEventList(List: TStrings); virtual;

    //class function Name: String; virtual;
    property Name: String read FName;
    //GlobalInstanceをエンジンに作成するかどうか
    //constructorで例外を起こす可能性のあるobjectはFalseにしてください
    class function IsMakeGlobalInstance: Boolean; virtual;
    //配列アクセス用仮想メソッド for..in文で使用します
    class function IsArray: Boolean; virtual;
    function GetCount: Integer; virtual;
    function GetItem(Index: Integer): TJValue; virtual;
  end;

  TJPrototypeObject = class(TJObject)
  private
    FPrototype: TJObject;
    function GetPrototype: TJObject;
    procedure SetPrototype(const Value: TJObject);
  protected
    procedure RegistMethods; override;
    procedure Notification(AObject: TJNotify); override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure GetPropertyList(List: TStringList); override;

    property Prototype: TJObject read GetPrototype write SetPrototype;
  end;

  TJVCLPersistent = class(TJObject)
  private
    function GetVCLClassName: String;
   protected
    FVCL: TPersistent;
    FCanDestroy: Boolean;

    function DoAssign(Param: TJValueList): TJValue; virtual;

    procedure RegistEvents; virtual;
    procedure CreateVCL; virtual;
    procedure DestroyVCL; virtual;
    procedure CreateObjects; virtual;
    procedure Error(Msg: String = ''); virtual;
    procedure ArgsError; virtual;
    procedure CheckVCL(Param: TJValueList = nil; ArgCount: Integer = 0); virtual;

    function GetValueImpl(S: String; var RetVal: TJValue; Param: TJValueList = nil): Boolean; override;
    function SetValueImpl(S: String; var Value: TJValue; Param: TJValueList = nil): Boolean; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    class function IsMakeGlobalInstance: Boolean; override;
    procedure GetPropertyList(List: TStringList); override;

    function RegistVCL(AVCL: TPersistent; ACanDestroy: Boolean): Boolean; virtual;
    class function VCLClassType: TClass; virtual;
    function IsVCL: Boolean;
    property GetVCL: TPersistent read FVCL;
  published
    property VCLclassName: String read GetVCLClassName;
  end;

  TJObjectList = class(TBinList)
  public
    procedure Clear; override;
  end;

  //object作成クラス
  TJObjectFactory = class(TJNotify)
  private
    FEngine: TJBaseEngine;
    FHash: TPointerHashTable;
    FItems: TJObjectList;
    FProto: TJHash;

    FOnNewObject: TNewObjectEvent;

    procedure HashOnItemDispose(Sender: TObject; P: PHashItem);
    function GetObjectCount: Integer;
    function GetObjectNameList: TStringList;
  protected
    procedure Notification(AObject: TJNotify); override;
  public
    constructor Create(AEngine: TJBaseEngine);
    destructor Destroy; override;

    function HasObject(ObjectName: String): Boolean;
    procedure ImportObject(ObjectName: String; ObjectClass: TJObjectClass);
    procedure DeleteObject(ObjectName: String);
    function GetObject(ObjectName: String): PJObjectClass;

    procedure Add(Obj: TJObject);
    procedure Clear;

    function GetPrototype(ObjectName: String): TJObject;
    function SetPrototype(ObjectName: String; Obj: TJObject): Boolean;

    property ObjectCount: Integer read GetObjectCount;
    property ObjectNameList: TStringList read GetObjectNameList;
    property OnNewObject: TNewObjectEvent read FOnNewObject write FOnNewObject;
  end;

  //ローカルスタック
  TJLocalSymbolTable = class(TJNotify)
  private
    FParent: TJLocalSymbolTable;
    FLocal: TJHash;
    FThis: TJObject;
    FTables: TObjectList;
    FTempObjects: TJValueList;

    function GetGlobalTable: TJGlobalSymbolTable;
    function GetThis: TJObject;
    procedure SetThis(const Value: TJObject);
    procedure SetParent(Value: TJLocalSymbolTable);
  protected
    procedure Notification(AObject: TJNotify); override;
    function SetValueImpl(Caller: TJLocalSymbolTable; Symbol: String; var Value: TJValue): Boolean; virtual;
    function GetValueImpl(Caller: TJLocalSymbolTable; Symbol: String; var Value: TJValue): Boolean; virtual;
  public
    constructor Create(AParent: TJLocalSymbolTable); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LocalCopy(Source: TJLocalSymbolTable); virtual;

    function GetValue(Symbol: String; var Value: TJValue): Boolean;
    procedure SetValue(Symbol: String; Value: TJValue; RegistType: TJRegistVarType);
    procedure RegistValue(Symbol: String; Value: TJValue);

    function GetGlobalValue(Symbol: String; var Value: TJValue): Boolean;
    procedure RegistGlobalValue(Symbol: String; Value: TJValue);
    procedure RegistStaticValue(Symbol: String; Value: TJValue);

    procedure AddTemporaryObject(AObject: TJObject; IncRef: Boolean = True);
    procedure ClearTemporaryObject;

    function PushLocalTable(ATable: TJLocalSymbolTable; AThis: TJObject): TJLocalSymbolTable; virtual;
    procedure PopLocalTable;
    function GetNodeTable: TJFunctionSymbolTable;

    property Parent: TJLocalSymbolTable read FParent write SetParent;
    property This: TJObject read GetThis write SetThis;
  end;

  //基本的に関数の接点を管理
  //ローカルをここから派生する、一番最後に作成したローカルが本命
  TJFunctionSymbolTable = class(TJLocalSymbolTable)
  protected
    function SetValueImpl(Caller: TJLocalSymbolTable; Symbol: String; var Value: TJValue): Boolean; override;
    function GetValueImpl(Caller: TJLocalSymbolTable; Symbol: String; var Value: TJValue): Boolean; override;
  end;

  TJGlobalSymbolTable = class(TJFunctionSymbolTable)
  end;

  TJRootSymbolTable = class(TJGlobalSymbolTable)
  private
    FFunctions: TObjectHashTable;
    FGlobals: TStringList;
  public
    constructor Create(AParent: TJLocalSymbolTable); override;
    destructor Destroy; override;
    procedure Clear; override;

    function GetFunctionTable(AParent,AFunc: PJStatement): TJFunctionSymbolTable; overload;
    function GetFunctionTable(AParent: TJFunctionSymbolTable; AFunc: PJStatement): TJFunctionSymbolTable; overload;
    function FindGlobalTable(AName: String): TJGlobalSymbolTable;
    function MakeGlobalTable(AName: String; AFunc: PJStatement): TJGlobalSymbolTable;
  end;

  TJBaseEngine = class(TObject)
  public
    function MakeObject(Name: String; Param: TJValueList): TJObject; virtual; abstract;
    procedure ImportObject(ObjectName: String; ObjectClass: TJObjectClass); virtual; abstract;
    function GetScriptFilename: String; virtual; abstract;
    function FindImportFilename(Filename: String; var FindedFilename: String): Boolean; virtual; abstract;
  end;



  //基本例外
  EJException = class(Exception)
  private
    FExceptName: String;
  public
    property ExceptName: String read FExceptName;
  end;


  EJAbort = class(EJException);
  EJStatement = class(EJException);
  EJBreak = class(EJStatement);
  EJContinue = class(EJStatement);
  //関数からのreturn
  EJReturn = class(EJStatement)
  private
    FValue: TJValue;
  public
    constructor Create(AValue: TJValue);
    property Value: TJValue read FValue;
  end;

  EJExit = class(EJStatement)
  private
    FStatus: Integer;
  public
    constructor Create(AStatus: Integer);
    property Status: Integer read FStatus;
  end;

  EJError = class(EJException);
  EJRefCountError = class(EJError);
  //実行時エラー
  EJRuntimeError = class(EJError)
  private
    FValue: TJValue;
  public
    constructor Create(AExceptName,AErrorMsg: String; AValue: PJValue = nil);
    property Value: TJValue read FValue;
  end;

  EJThrow = class(EJRuntimeError);

  EJSyntaxError = class(EJRuntimeError)
  private
    FLineNo: Integer;
  public
    constructor Create(ALineNo: Integer; AMsg: String; AValue: PJValue = nil);
    property LineNo: Integer read FLineNo;
  end;


const
  //例外名
  E_EXCEPTION = 'Exception';
  E_THROW = 'EThrow';
  E_CALL = 'ECallError';
  E_INDEX = 'EIndexError';
  E_KEY = 'EKeyError';
  E_IO = 'EIOError';
  E_FILE = 'EFileError';
  E_DIR = 'EDirectoryError';
  E_NAME = 'ENameError';
  E_TYPE = 'ETypeError';
  E_MATHR = 'EMathError';
  E_ZD = 'EZDError';
  E_EOF = 'EEOFError';
  E_SOCKET = 'ESocketError';
  E_REGEXP = 'ERegExp';
  E_STRINGS = 'EStringsError';
  E_WIN32 = 'EWin32Error';
  E_INI = 'EIniError';
  E_CRC = 'ECRCError';
  E_BASE64 = 'EBase64Error';
  E_PROP = 'EPropertyError';
  E_ACTIVEX = 'EActiveXError';
  E_SYNTAX = 'ESyntaxError';
  E_DLL = 'EDLLLoadError';
  E_DYNACALL = 'EDynaCallError';
  E_STRING = 'EStringError';
  E_DELETE = 'EDeleteError';
  E_ENUMERATOR = 'EEnumeratorError';
  E_CONVERT = 'EConvertError';
  E_VCL = 'EVCLError';
  E_STRUCT = 'EStructError';

  E_UNKNOWN_LINE_NO = -1;


//補助関数
function IsConstant(P: PJExpr): Boolean;
function IsVariable(P: PJExpr): Boolean;
function ConstantValueInt(P: PJExpr): Integer;

procedure EmptyValue(var V: TJValue);
procedure EmptyFunction(var Func: IJFunction);
function TypeOf(P: PJValue): String;

function IsUndefined(P: PJValue): Boolean;
function IsNull(P: PJValue): Boolean;
function IsInteger(P: PJValue): Boolean;
function IsDouble(P: PJValue): Boolean;
function IsNumber(P: PJValue): Boolean;
function IsString(P: PJValue): Boolean;
function IsRegExp(P: PJValue): Boolean;
function IsObject(P: PJValue): Boolean;
function IsNumberObject(P: PJValue): Boolean;
function IsStringObject(P: PJValue): Boolean;
function IsRegExpObject(P: PJValue): Boolean;
function IsArrayObject(P: PJValue): Boolean;
function IsVCLObject(P: PJValue): Boolean;

function IsBool(P: PJValue): Boolean;
function IsFunction(P: PJValue): Boolean;
function IsInfinity(P: PJValue): Boolean;
function IsNaN(P: PJValue): Boolean;
function IsDispatch(P: PJValue): Boolean;
function IsNameSpace(P: PJValue): Boolean;
function IsConstructor(P: PJValue): Boolean;
function IsClass(P: PJValue): Boolean;
function IsEvent(P: PJValue): Boolean;

function TryAsNumber(P: PJValue): Boolean;
function EqualFunction(L,R: PJValue): Boolean;
function EqualType(L,R: PJValue): Boolean;

function AsInteger(P: PJValue): Integer;
function AsDouble(P: PJValue): Double;
function AsString(P: PJValue): String;
function AsBool(P: PJValue): Boolean;
function AsDispatch(P: PJValue): IDispatch;
function AsSingle(P: PJValue): Single;
function AsChar(P: PJValue): Char;

function BuildUndefined: TJValue;
function BuildString(const V: String): TJValue;
function BuildNull: TJValue;
function BuildInteger(V: Integer): TJValue;
function BuildDouble(V: Double): TJValue;
function BuildObject(V: TJObject): TJValue;
function BuildBool(V: Boolean): TJValue;
function BuildInfinity(Negative: Boolean): TJValue;
function BuildNaN: TJValue;
function BuildDispatch(V: IDispatch): TJValue;
function BuildEvent(V: TMethod): TJValue;
function BuildFunction(V: IJFunction): TJValue;

{$IFNDEF NO_ACTIVEX}
function VariantToValue(const V: OleVariant; const Engine: TJBaseEngine): TJValue;
function ValueToVariant(const V: TJValue): OleVariant;
{$ENDIF}

function VarRecToValue(const V: TVarRec): TJValue;
function ValueToVarRec(const V: TJValue): TVarRec;
procedure DisposeVarRec(Rec: TVarRecArray);

function GetParamCount(const Param: TJValueList): Integer;
function IsParam1(const Param: TJValueList): Boolean;
function IsParam2(const Param: TJValueList): Boolean;
function IsParam3(const Param: TJValueList): Boolean;
function IsParam4(const Param: TJValueList): Boolean;

procedure HashToJObject(Hash: TStringHashTable; JObject: TJObject);
procedure JObjectToHash(JObject: TJObject; Hash: TStringHashTable);

{$IFNDEF NO_ACTIVEX}
function AXMethodFlagToDisp(A: TJActiveXMethodFlag): Word;
function AXMethodFlagToString(A: TJActiveXMethodFlag): String;
{$ENDIF}

function ValueListToDynaValueArray(Format: String; const Param: TJValueList): TDynaValueArray;
function DynaResultToValue(Format: String; const DynaResult: TDynaResult): TJValue;
procedure SetRefDynaValue(const DynaValueArray: TDynaValueArray; Param: TJValueList);

//LとRの計算結果を返す
function CalcValue1(Code: TJOPCode; const L: TJValue): TJValue;
function CalcValue2(Code: TJOPCode; const L,R: TJValue): TJValue;
function CalcValue3(Code: TJOPCode; const L,R,T: TJValue): TJValue;
function AssignValue(Code: TJOPCode; const Variable,Value: TJValue): TJValue;
function CompareValue(Code: TJOPCode; const L,R: TJValue): TJValue;

//Object
function GetDefaultProperty(Obj: TObject; const Prop: String; var Value: TJValue; Engine: TJBaseEngine): Boolean;
function SetDefaultProperty(Obj: TObject; const Prop: String; Value: TJValue; ValueTypeInfo: PTypeInfo = nil): Boolean;
procedure GetDefaultProperties(Obj: TObject; PropNames: TStrings; Event: Boolean = False);
procedure SetDefaultMethodNil(Obj: TObject);
procedure EnumMethodNames(Obj: TObject; List: TStrings);

//集合型
procedure EnumSetNames(Info: PTypeInfo; List: TStrings);
procedure SetToJObject(Info: PTypeInfo; Obj: TJObject; const Value);
procedure JObjectToSet(Info: PTypeInfo; Obj: TJObject; var RetValue);
function JObjectToSetStr(Info: PTypeInfo; Obj: TJObject): String;
function SetToStr(Info: PTypeInfo; const Value): string;
procedure StrToSet(Info: PTypeInfo; const S: String; var RetValue);
//列挙型
function ValueToEnum(Info: PTypeInfo; var Value: TJValue; var Enum: Integer): Boolean;
function EnumToValue(Info: PTypeInfo; var Value: TJValue; Enum: Integer): Boolean;



implementation

uses
  ecma_engine,
{$IFNDEF NO_ACTIVEX}
  ecma_activex,
{$ENDIF}
{$IFNDEF NO_EXTENSION}
  ecma_extobject,
{$ENDIF}
{$IFNDEF NO_VCL}
  ecma_vcl,
{$ENDIF}
  ecma_object;


function GetParamCount(const Param: TJValueList): Integer;
begin
  if Assigned(Param) then
    Result := Param.Count
  else
    Result := 0;
end;

function IsParam1(const Param: TJValueList): Boolean;
begin
  Result := (Assigned(Param) and (Param.Count > 0))
end;

function IsParam2(const Param: TJValueList): Boolean;
begin
  Result := (Assigned(Param) and (Param.Count > 1))
end;

function IsParam3(const Param: TJValueList): Boolean;
begin
  Result := (Assigned(Param) and (Param.Count > 2))
end;

function IsParam4(const Param: TJValueList): Boolean;
begin
  Result := (Assigned(Param) and (Param.Count > 3))
end;

function IsConstant(P: PJExpr): Boolean;
//定数かどうか？
begin
  Result := (Assigned(P) and (P^.Code = opConstant))
end;

function IsVariable(P: PJExpr): Boolean;
//変数かどうか？
begin
  Result := (Assigned(P) and (P^.Code = opVariable))
end;

function ConstantValueInt(P: PJExpr): Integer;
//定数の整数値を得る
begin
  Result := 0;
  if IsConstant(P) and (P^.Value.ValueType = vtInteger) then
    Result := P^.Value.vInteger;
end;

procedure EmptyValue(var V: TJValue);
//変数を初期化
begin
  V.ValueType := vtUndefined;
  //V.Attributes := [];
  V.vDouble := 0;
  V.vInteger := 0;
  V.vString := '';
  V.vDispatch := nil;
  V.vFunction := nil;
end;

procedure EmptyFunction(var Func: IJFunction);
begin
  //作成する
  Func := TJFunctionImpl.Create;
  {
  Func.Symbol := '';
  Func.FuncType := ftStatement;
  Func.Parameter := nil;
  Func.vStatement := nil;
  Func.Flag := fcfNone;
  Func.vActiveX.Parent := nil;
  Func.vActiveX.Dispid := 0;
  Func.vActiveX.Flag := axfMethod;
  ClearDynaDeclare(Func.vDynaCall^);
  Func.FunctionTable := nil;
  Func.LocalTable := nil;
  Func.MethodOwner := nil;
  }
end;

function AsInteger(P: PJValue): Integer;
//整数値を返す
begin
  Result := 0;
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtUndefined: Result := 0;
    vtInteger: Result := P^.vInteger;
    vtDouble: Result := Round(P^.vDouble);
    vtString: Result := StrToIntDef(P^.vString,0);
    vtBool: Result := Integer(P^.vBool);
    vtNull: Result := 0;
    vtObject: Result := Round(P^.vObject.ToNumber);
    vtFunction:;
  end;
end;

function AsChar(P: PJValue): Char;
begin
  Result := #0;
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtUndefined: Result := #0;
    vtInteger: Result := Char(P^.vInteger);
    vtDouble: Result := Char(Trunc(P^.vDouble));
    vtBool: Result := Char(P^.vBool);
    vtNull: Result := #0;
    vtString:
    begin
      if Length(P^.vString) > 0 then
        Result := P^.vString[1];
    end;
    vtObject: Result := P^.vObject.ToChar;
  end;
end;

function AsSingle(P: PJValue): Single;
begin
  Result := AsDouble(P);
end;

function AsDouble(P: PJValue): Double;
//doubleを返す
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtInteger: Result := P^.vInteger;
    vtDouble: Result := P^.vDouble;
    vtBool: Result := Integer(P^.vBool);
    vtString:
    begin
      try
        Result := StrToFloat(P^.vString);
      except
        try
          i := StrToInt(P^.vString);
          Result := i;
        except
        end;
      end;
    end;

    vtObject: Result := P^.vObject.ToNumber;
  end;

end;

function TryAsNumber(P: PJValue): Boolean;
//数値に出来る？
var
  s: string;
begin
  Result := False;
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtInteger: Result := True;
    vtDouble: Result := True;
    vtBool: Result := True;
  else
    if IsString(P) then
    begin
      s := AsString(P);
      try
        StrToFloat(s);
        Result := True;
      except
        try
          StrToInt(s);
          Result := True;
        except
        end;
      end;
    end
    else
      Result := IsNumberObject(P);
  end;//case
end;

function AsString(P: PJValue): String;
//文字にして返す
begin
  Result := '';
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtUndefined: Result := 'undefined';
    vtInteger: Result := IntToStr(P^.vInteger);
    vtDouble: Result := FloatToStr(P^.vDouble);
    vtString: Result := P^.vString;
    vtRegExp: Result := '/' + P^.vString + '/' + P^.vRegExpOptions;
    vtNull: Result := 'null';
    vtBool:
    begin
      if P^.vBool then
        Result := 'true'
      else
        Result := 'false';
    end;
    vtObject: Result := P^.vObject.ToString;
    vtFunction:
    begin
      Result := 'function ' + P^.vFunction.Symbol
    end;
    vtInfinity:
    begin
      if P^.vBool then
        Result := '-infinity'
      else
        Result := 'infinity';
    end;
    vtNaN: Result := 'NaN';
    vtDispatch: Result := 'dispatch' + IntToStr(Integer(P^.vDispatch));
  end;
end;

function AsBool(P: PJValue): Boolean;
//bool値を返す
begin
  Result := False;
  if not Assigned(P) then
    Exit;

  case P^.ValueType of
    vtUndefined: Result := False;
    vtInteger: Result := (P^.vInteger <> 0);
    vtDouble: Result := (Trunc(P^.vDouble) <> 0);
    vtString: Result := (P^.vString <> '');
    vtRegExp: Result := (P^.vString <> '');
    vtNull: Result := False;
    vtBool: Result := P^.vBool;
    vtFunction: Result := True;
    vtInfinity: Result := True;
    vtNaN: Result := True;
    vtObject: Result := P^.vObject.ToBool;
    vtDispatch: Result := Assigned(P^.vDispatch);
  end;
end;

function AsDispatch(P: PJValue): IDispatch;
begin
  Result := nil;
  if not Assigned(P) then
    Exit;

  if IsDispatch(P) then
    Result := P^.vDispatch;
end;

function TypeOf(P: PJValue): String;
begin
  if IsInteger(P) or IsDouble(P) or IsNaN(P) or IsInfinity(P) then
    Result := 'number'
  else if IsObject(P) then //stringよりobjectを先にする
    Result := 'object'
  else if IsString(P) then
    Result := 'string'
  else if IsRegExp(P) then
    Result := 'regexp'
  else if IsBool(P) then
    Result := 'boolean'
  else if IsFunction(P) then
    Result := 'function'
  else if IsUndefined(P) then
    Result := 'undefined'
  else if IsNull(P) then
    Result := 'null'
  else if IsDispatch(P) then
    Result := 'dispatch'
  else if IsEvent(P) then
    Result := 'event'
  else
    Result := '';
end;

function IsUndefined(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtUndefined));
end;

function IsNull(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtNull));
end;

function IsInteger(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtInteger));
end;

function IsDouble(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtDouble));
end;

function IsNumber(P: PJValue): Boolean;
//実際に数値を持つ値かどうか
begin
  //NaN,infinityはFalseを返す
  Result := Assigned(P) and
            ((P^.ValueType in [vtInteger,vtDouble]) or IsNumberObject(P));
end;

function IsString(P: PJValue): Boolean;
begin
  Result := Assigned(P) and ((P^.ValueType = vtString) or
{$IFNDEF NO_EXTENSION}
                             IsStringBufferObject(P) or
{$ENDIF}
                             IsStringObject(P));
end;

function IsRegExp(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtRegExp));
end;

function IsObject(P: PJValue): Boolean;
begin
  Result := Assigned(P) and
            (P^.ValueType = vtObject) and
            Assigned(P^.vObject);
end;

function IsNumberObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJNumberObject);
end;

function IsStringObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJStringObject);
end;

function IsRegExpObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJRegExpObject);
end;

function IsArrayObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and P^.vObject.IsArray;
end;

function IsVCLObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJVCLPersistent);
end;

function IsBool(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtBool));
end;

function IsFunction(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtFunction));
end;

function IsNameSpace(P: PJValue): Boolean;
begin
  Result := IsFunction(P) and (P^.vFunction.FuncType = ftImport);
end;

function IsConstructor(P: PJValue): Boolean;
begin
  Result := IsFunction(P) and(P^.vFunction.FuncType = ftStatement);
end;

function IsClass(P: PJValue): Boolean;
begin
  Result := IsFunction(P) and(P^.vFunction.FuncType = ftClass);
end;

function IsInfinity(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtInfinity));
end;

function IsNaN(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtNaN));
end;

function IsDispatch(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtDispatch));
end;

function IsEvent(P: PJValue): Boolean;
begin
  Result := (Assigned(P) and (P^.ValueType = vtEvent));
end;

function BuildUndefined: TJValue;
begin
  EmptyValue(Result);
end;

function BuildString(const V: String): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtString;
  Result.vString := V;
end;

function BuildNull: TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtNull;
  Result.vNull := nil;
end;

function BuildInteger(V: Integer): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtInteger;
  Result.vInteger := V;
end;

function BuildDouble(V: Double): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtDouble;
  Result.vDouble := V;
end;

function BuildObject(V: TJObject): TJValue;
begin
  EmptyValue(Result);
  if Assigned(V) then
  begin
    Result.ValueType := vtObject;
    Result.vObject := V;
  end
  else
    Result := BuildNull;
end;

function BuildBool(V: Boolean): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtBool;
  Result.vBool := V;
end;

function BuildInfinity(Negative: Boolean): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtInfinity;
  Result.vBool := Negative; // 負の無限大ならTrue
end;

function BuildNaN: TJValue;
begin
  Emptyvalue(Result);
  Result.ValueType := vtNaN;
end;

function BuildDispatch(V: IDispatch): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtDispatch;
  Result.vDispatch := V;
end;

function BuildEvent(V: TMethod): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtEvent;
  Result.vEvent := V;
end;

function BuildFunction(V: IJFunction): TJValue;
begin
  EmptyValue(Result);
  Result.ValueType := vtFunction;
  Result.vFunction := V;
end;

{$IFNDEF NO_ACTIVEX}
function VariantToValue(const V: OleVariant; const Engine: TJBaseEngine): TJValue;
//variantから変換
var
  act: TJActiveXObject;
  date: TJDateObject;
begin
  EmptyValue(Result);
  case VarType(V) of
    varNull: Result := BuildNull;
    varSmallint,varInteger,varByte: Result := BuildInteger(V);
    varSingle,varDouble: Result := BuildDouble(V);
    varOleStr,varString,varVariant: Result := BuildString(V);
    varBoolean: Result := BuildBool(V);
    varDispatch:
    begin
      //ActiveXObjectに変換
      if Assigned(Engine) then
      begin
        act := TJActiveXObject.Create(Engine);
        act.disp := V;
        Result := BuildObject(act);
      end
      else
        Result := BuildDispatch(V);
    end;
    varDate:
    begin
      //Dateに変換
      if Assigned(Engine) then
      begin
        date := TJDateObject.Create(Engine);
        date.LocalTime := V;
        Result := BuildObject(date);
      end;
    end;
  end;
end;

function ValueToVariant(const V: TJValue): OleVariant;
//variantへ変換
var
  ws: WideString;
  tmp: TJValue;
begin
  //VarClear(Result);  varClearはバグっている
  VariantInit(Result);
  case V.ValueType of
    vtNull: Result := VarAsType(Result,varNull);
    vtInteger: Result := V.vInteger;
    vtDouble: Result := V.vDouble;
    vtBool: Result := V.vBool;
    vtString:
    begin
      ws := V.vString;
      Result := ws;
    end;
    vtDispatch: Result := V.vDispatch;
    vtObject:
    begin
      if V.vObject is TJActiveXObject then
      begin
        Result := (V.vObject as TJActiveXObject).disp;
      end
      else if (V.vObject is TJStringObject)
      {$IFNDEF NO_EXTENSION}
           or (V.vObject is TJStringBufferObject)
      {$ENDIF}
           then
      begin
        ws := V.vObject.ToString;
        Result := ws;
      end
      else if V.vObject is TJDateObject then
      begin
        Result := (V.vObject as TJDateObject).LocalTime;
      end
      else if V.vObject is TJNumberObject then
      begin
        tmp := (V.vObject as TJNumberObject).ValueOf;
        if tmp.ValueType = vtInteger then
          Result := tmp.vInteger
        else
          Result := tmp.vDouble;
      end
      else if V.vObject is TJBooleanObject then
      begin
        Result := V.vObject.ToBool;
      end;
    end;
  end;
end;
{$ENDIF}

function VarRecToValue(const V: TVarRec): TJValue;
//TVarRecを変換する
begin
  EmptyValue(Result);
  case V.VType of
    system.vtInteger: Result := BuildInteger(V.VInteger);
    system.vtInt64: Result := BuildDouble(V.VInt64^);

    system.vtBoolean: Result := BuildBool(V.VBoolean);

    system.vtExtended: Result := BuildDouble(V.VExtended^);

    system.vtString: Result := BuildString(V.VString^);
    system.vtChar: Result := BuildString(V.VChar);
    system.vtWideChar: Result := BuildString(V.VWideChar);
    system.vtAnsiString: Result := BuildString(AnsiString(V.VAnsiString));
    system.vtWideString: Result := BuildString(WideString(V.VWideString));
    system.vtPChar: Result := BuildString(V.VPChar);
    system.vtPWideChar: Result := BuildString(V.VWideChar);

    system.vtObject:
    begin
      if V.VObject is TJObject then
        Result := BuildObject(V.VObject as TJObject);
    end;
{$IFNDEF NO_ACTIVEX}
    system.vtVariant: Result := VariantToValue(V.VVariant^,nil);
{$ENDIF}

    //system.vtInterface:
    //system.vtPointer:
    //system.vtClass:
    //system.vtCurrency:
  end;
end;

function ValueToVarRec(const V: TJValue): TVarRec;
//TVarRecから変換する
var
  p: PChar;
  s: String;
begin
  case V.ValueType of
    vtInteger:
    begin
      Result.VType := system.vtInteger;
      Result.VInteger := AsInteger(@V);
    end;

    vtDouble:
    begin
      Result.VType := system.vtExtended;
      New(Result.VExtended);
      Result.VExtended^ := AsDouble(@V);
    end;

    vtString,vtObject,vtBool,vtNull,vtUndefined,
    vtDispatch,vtNaN,vtInfinity,vtFunction,vtRegExp:
    begin
      Result.VType := system.vtPChar;
      s := AsString(@v);
      GetMem(p,Length(s) + 1);
      FillChar(p^,Length(s) + 1,0);
      StrPLCopy(p,s,Length(s));
      Result.VPChar := p;
    end;
  else
    Result.VType := system.vtInteger;
    Result.VInteger := 0;
  end;
  sleep(0);
end;

procedure DisposeVarRec(Rec: TVarRecArray);
//TVarRecを開放する
var
  i: Integer;
begin
  for i := 0 to Length(Rec) - 1 do
    case Rec[i].VType of
      system.vtExtended: Dispose(Rec[i].VExtended);
      system.vtPChar: FreeMem(Rec[i].VPChar);
    end;
end;

function EqualFunction(L,R: PJValue): Boolean;
//同じ関数かどうか
begin
  Result := False;
  if IsFunction(L) and IsFunction(R) then
    Result := L^.vFunction  = R^.vFunction;
end;

function EqualType(L,R: PJValue): Boolean;
//同じ型かどうか
begin
  Result := False;
  if Assigned(L) and Assigned(R) then
    Result := L^.ValueType = R^.ValueType;
end;

procedure HashToJObject(Hash: TStringHashTable; JObject: TJObject);
//hashからobjectへ
var
  keys: TStringList;
  i: Integer;
begin
  keys := Hash.KeyList;
  JObject.ClearProperties;
  for i := 0 to keys.Count - 1 do
    JObject.RegistProperty(keys[i],BuildString(Hash[keys[i]]));
end;

procedure JObjectToHash(JObject: TJObject; Hash: TStringHashTable);
//objectからhashへ
var
  keys: TStringList;
  i: Integer;
  v: TJValue;
begin
  keys := TStringList.Create;
  try
    JObject.GetPropertyList(keys);
    Hash.Clear;
    for i := 0 to keys.Count - 1 do
    begin
      v := JObject.GetValue(keys[i],True);
      Hash[keys[i]] := AsString(@v);
    end;
  finally
    keys.Free;
  end;
end;

{$IFNDEF NO_ACTIVEX}

function AXMethodFlagToDisp(A: TJActiveXMethodFlag): Word;
//ActveXのメソッドフラグを変換
begin
  Result := DISPATCH_METHOD or DISPATCH_PROPERTYGET;
  case A of
    axfGet: Result := DISPATCH_PROPERTYGET or DISPATCH_METHOD;
    axfPut: Result := DISPATCH_PROPERTYPUT;
  end;
end;

function AXMethodFlagToString(A: TJActiveXMethodFlag): String;
//activexの文字列を返す
begin
  Result := 'DISPATCH_METHOD';
  case A of
    axfGet: Result := 'DISPATCH_PROPERTYGET';
    axfPut: Result := 'DISPATCH_PROPERTYPUT';
  end;
end;
{$ENDIF}

function ValueListToDynaValueArray(
  Format: String; const Param: TJValueList): TDynaValueArray;
//valuelsitを変換する
var
  len,i: Integer;
  v: TJValue;

  function IsRefNull(P: PJValue): Boolean;
  //nullかどうか
  begin
    if Assigned(P) then
      Result := P^.ValueType in [vtUndefined,vtNull]
    else
      Result := True;
  end;

begin
  len := Length(Format);
  if len > 0 then
  begin
    Format := LowerCase(Format);
    SetLength(Result,len);
    for i := 1 to len do
    begin
      try
        v := Param[i - 1];
      except
        on EListError do
          v := BuildNull;
      end;

      case Format[i] of
        'c':
        begin
          Result[i - 1].VType := dvtChar;
          Result[i - 1]._char := AsChar(@v);
        end;

        '1':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtRefChar;
            Result[i - 1]._char := AsChar(@v);
          end;
        end;

        't':
        begin
          Result[i - 1].VType := dvtShort;
          Result[i - 1]._short := AsInteger(@v);
        end;

        '2':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtRefShort;
            Result[i - 1]._short := AsInteger(@v);
          end;
        end;

        'l','h','p','u','b':
        begin
          Result[i - 1].VType := dvtLong;
          Result[i - 1]._long := AsInteger(@v);
        end;

        '4':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtRefLong;
            Result[i - 1]._long := AsInteger(@v);
          end;
        end;

        'i':
        begin
          Result[i - 1].VType := dvtInt64;
          Result[i - 1]._int64 := Round(AsDouble(@v));
        end;

        '8':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtRefInt64;
            Result[i - 1]._int64 := Round(AsDouble(@v));
          end;
        end;

        's':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtString;
            Result[i - 1]._string := AsString(@v);
          end;
        end;

        'w':
        begin
          if IsRefNull(@v) then
          begin
            Result[i - 1].VType := dvtPointer;
            Result[i - 1]._long := 0;
          end
          else begin
            Result[i - 1].VType := dvtWideString;
            Result[i - 1]._widestring := AsString(@v);
          end;
        end;

        'f':
        begin
          Result[i - 1].VType := dvtFloat;
          Result[i - 1]._float := AsSingle(@v);
        end;

        'd':
        begin
          Result[i - 1].VType := dvtDouble;
          Result[i - 1]._double := AsDouble(@v);
        end;

        'a':
        begin
          Result[i - 1].VType := dvtIDispatch;
          Result[i - 1]._idispatch := AsDispatch(@v);
        end;

        'k':
        begin
          Result[i - 1].VType := dvtIUnknown;
          Result[i - 1]._iunknown := AsDispatch(@v);
        end;
      else
        //当てはまらない場合は例外
        raise EJThrow.Create(E_DYNACALL,'arguments flag error');
      end;
    end;
  end
  else
    Result := nil;
end;

function DynaResultToValue(Format: String; const DynaResult: TDynaResult): TJValue;
//dynacallの戻り値
var
  len: Integer;
begin
  Result := BuildNull;

  len := Length(Format);
  if len > 0 then
  begin
    Format := LowerCase(Format);
    case Format[1] of
      'c','t','l','h','p','u': Result := BuildInteger(DynaResult._long);
      'b': Result := BuildBool(DynaResult._long <> 0);
      'i': Result := BuildDouble(DynaResult._int64);
      'a','k': Result := BuildDispatch(IDispatch(DynaResult._long));
      's': Result := BuildString(PChar(DynaResult._pointer));
      'w': Result := BuildString(PWideChar(DynaResult._pointer));
      'd': Result := BuildDouble(DynaResult._double);
      'f': Result := BuildDouble(DynaResult._float);
    end;
  end;
end;

procedure SetRefDynaValue(const DynaValueArray: TDynaValueArray; Param: TJValueList);
//参照渡しの値を反映する
var
  cnt,i: Integer;
  v: TJValue;
begin
  cnt := GetParamCount(Param);
  //小さい方に合わせる
  if cnt > Length(DynaValueArray) then
    cnt := Length(DynaValueArray);

  for i := 0 to cnt - 1 do
  begin
    v := Param[i];
    //値を反映するのはNumberオブジェクトだけ
    if IsNumberObject(@v) then
      case DynaValueArray[i].VType of
        dvtRefChar:
          (v.vObject as TJNumberObject).int := Integer(DynaValueArray[i]._char);
        dvtRefShort:
          (v.vObject as TJNumberObject).int := DynaValueArray[i]._short;
        dvtRefLong:
          (v.vObject as TJNumberObject).int := DynaValueArray[i]._long;
        dvtRefInt64:
          (v.vObject as TJNumberObject).number := DynaValueArray[i]._int64;
      end;
  end;
end;

function CalcValue1(Code: TJOPCode; const L: TJValue): TJValue;
//１次式
//Lの計算結果を返す
//型によっては計算できない場合がある
begin
  EmptyValue(Result);
  //型チェックをする
  //未定義エラー
  if IsUndefined(@L) then
    raise EJThrow.Create(E_NAME,'')
  //型エラー
  else if IsFunction(@L) or IsNaN(@L) then
    raise EJThrow.Create(E_TYPE,GetEnumName(TypeInfo(TJValueType),Ord(L.ValueType)));
  //計算
  case Code of
    opMinus:
    begin
      if IsDouble(@L) then
        Result := BuildDouble(0 - AsDouble(@L))
      else if IsInfinity(@L) then
        Result := BuildInfinity(not L.vBool)
      else
        Result := BuildInteger(0 - (AsInteger(@L)));
    end;
    opPlus:
    begin
      if IsDouble(@L) or IsInfinity(@L) then
        Result := L //そのまま返す
      else
        Result := BuildInteger(AsInteger(@L));
    end;
    opBitNot:
    begin
      if IsInfinity(@L) then
        raise EJThrow.Create(E_TYPE,GetEnumName(TypeInfo(TJValueType),Ord(L.ValueType)))
      else
        Result := BuildInteger(not (AsInteger(@L)));
    end;
  end;
end;

function CalcValue2(Code: TJOPCode; const L,R: TJValue): TJValue;
//２次式
//LとRの計算結果を返す
//型によっては計算できない場合がある
var
  dl,dr: Double;
  il,ir: Integer;
begin
  EmptyValue(Result);
  //型チェックをする
  //未定義エラー
  if IsUndefined(@L) or
     IsUndefined(@R) then
    raise EJThrow.Create(E_NAME,'');
  //型エラー
  if IsFunction(@L) or IsInfinity(@L) or IsNaN(@L) or
     IsFunction(@R) or IsInfinity(@R) or IsNaN(@R) then
    raise EJThrow.Create(E_TYPE,
      GetEnumName(TypeInfo(TJValueType),Ord(L.ValueType)) + ' - ' +
      GetEnumName(TypeInfo(TJValueType),Ord(R.ValueType))
      );
  //計算
  case Code of
    opAdd: //加算    +
    begin
      //どちらかが文字の場合は文字に変換
      if IsString(@L) or IsString(@R) then
        Result := BuildString(AsString(@L) + AsString(@R))
{ TODO : integerとdoubleのどちらを優先するか }
      {
      else
        try
          Result := BuildDouble(AsDouble(@L) + AsDouble(@R));
          if IsInteger(@L) and IsInteger(@R) and
             (Result.vDouble <= MaxInt) and (Result.vDouble >= Low(Integer)) then
            Result := BuildInteger(AsInteger(@Result));
        except
          Result := BuildInfinity(AsDouble(@L) < 0);
        end;
      }
      {
      //どちらかがdoubleの場合は変換
      else if IsDouble(@L) or IsDouble(@R) then
        try
          Result := BuildDouble(AsDouble(@L) + AsDouble(@R))
        except
          Result := BuildInfinity(AsDouble(@L) < 0);
        end
      else //整数化
        Result := BuildInteger(AsInteger(@L) + AsInteger(@R));
      }
      //{
      else
        try
          //if IsInteger(@L) and IsInteger(@R) then
          //  Result := BuildInteger(L.vInteger + R.vInteger)
          //else
            Result := BuildDouble(AsDouble(@L) + AsDouble(@R));
        except
          Result := BuildInfinity(AsDouble(@L) < 0);
        end;
      //}
    end;
    opSub: //減算       -
    begin
      {
      try
        Result := BuildDouble(AsDouble(@L) - AsDouble(@R));
        if IsInteger(@L) and IsInteger(@R) and
           (Result.vDouble <= MaxInt) and (Result.vDouble >= Low(Integer)) then
          Result := BuildInteger(AsInteger(@Result));
      except
        Result := BuildInfinity(AsDouble(@L) < 0);
      end;
      }
      {
      if IsDouble(@L) or IsDouble(@R) then
        try
          Result := BuildDouble(AsDouble(@L) - AsDouble(@R))
        except
          Result := BuildInfinity(AsDouble(@L) < 0);
        end
      else //整数化
        Result := BuildInteger(AsInteger(@L) - AsInteger(@R));
      }
      //{
      try
        //if IsInteger(@L) and IsInteger(@R) then
        //  Result := BuildInteger(L.vInteger - R.vInteger)
        //else
          Result := BuildDouble(AsDouble(@L) - AsDouble(@R));
      except
        Result := BuildInfinity(AsDouble(@L) < 0);
      end;
      //}
    end;
    opMul: //掛け算    *
    begin
      {
      try
        Result := BuildDouble(AsDouble(@L) * AsDouble(@R));
        if IsInteger(@L) and IsInteger(@R) and
           (Result.vDouble <= MaxInt) and (Result.vDouble >= Low(Integer)) then
          Result := BuildInteger(AsInteger(@Result));
      except
        Result := BuildInfinity((AsDouble(@L) < 0) xor (AsDouble(@R) < 0));
      end;
      }
      {
      if IsDouble(@L) or IsDouble(@R) then
        try
          Result := BuildDouble(AsDouble(@L) * AsDouble(@R))
        except
          Result := BuildInfinity((AsDouble(@L) < 0) xor (AsDouble(@R) < 0));
        end
      else //整数化
        Result := BuildInteger(AsInteger(@L) * AsInteger(@R));
      }
      //{
      try
        //if IsInteger(@L) and IsInteger(@R) then
        //  Result := BuildInteger(L.vInteger * R.vInteger)
        //else
          Result := BuildDouble(AsDouble(@L) * AsDouble(@R));
      except
        Result := BuildInfinity((AsDouble(@L) < 0) xor (AsDouble(@R) < 0));
      end;
      //}
    end;
    opDiv: //割り算   /  0除算チェック
    begin
      {dbl := AsDouble(@R);
      if dbl = 0 then
        raise EJThrow.Create(E_ZD,'');

      try
        Result := BuildDouble(AsDouble(@L) / dbl);
      except
        Result := BuildInfinity((AsDouble(@L) < 0) xor (dbl < 0));
      end;}
      try
        Result := BuildDouble(AsDouble(@L) / AsDouble(@R));
      except
        dl := AsDouble(@L);
        dr := AsDouble(@R);
        if (dl = 0) and (dr = 0) then
          //両方0ならNaN
          Result := BuildNaN
        else
          //0除算の場合もInfinityを返す
          Result := BuildInfinity((dl < 0) xor (dr < 0));
      end;
    end;
    opDivInt: //割り算  div  0除算チェック
    begin
      {i := AsInteger(@R);
      if i = 0 then
        raise EJThrow.Create(E_ZD,'');

      Result := BuildInteger(AsInteger(@L) div i);}
      try
        Result := BuildInteger(AsInteger(@L) div AsInteger(@R));
      except
        il := AsInteger(@L);
        ir := AsInteger(@R);
        if (il = 0) and (ir = 0) then
          //両方0ならNaN
          Result := BuildNaN
        else
          //0除算の場合もInfinityを返す
          Result := BuildInfinity((il < 0) xor (ir < 0));
      end;
    end;
    opMod: //あまり %     0除算チェック
    begin
      {i := AsInteger(@R);
      if i = 0 then
        raise EJThrow.Create(E_ZD,'');

      Result := BuildInteger(AsInteger(@L) mod AsInteger(@R));}
      try
        Result := BuildInteger(AsInteger(@L) mod AsInteger(@R));
      except
        //例外時はNaNを返す
        Result := BuildNaN;
      end;
    end;
    opBitAnd: // &
    begin
      Result := BuildInteger(AsInteger(@L) and AsInteger(@R));
    end;
    opBitOr:  // |
    begin
      Result := BuildInteger(AsInteger(@L) or AsInteger(@R));
    end;
    opBitXor: // ^
    begin
      Result := BuildInteger(AsInteger(@L) xor AsInteger(@R));
    end;
    opBitLeft: // <<
    begin
      Result := BuildInteger(AsInteger(@L) shl AsInteger(@R));
    end;
    opBitRight: // >>
    begin
      Result := BuildInteger(AsInteger(@L) shr AsInteger(@R));
    end;
    opBitRightZero: // >>>
    begin
      Result := BuildInteger(Cardinal(AsInteger(@L)) shr AsInteger(@R));
    end;
  end;
end;

function CalcValue3(Code: TJOPCode; const L,R,T: TJValue): TJValue;
//3次式
begin
  EmptyValue(Result);
  case Code of
    opConditional:  //  l ? r : t
    begin
      if AsBool(@L) then
        Result := R
      else
        Result := T;
    end;
  end;
end;

function AssignValue(Code: TJOPCode; const Variable,Value: TJValue): TJValue;
//代入式
begin
  EmptyValue(Result);
  case Code of
    opMulAssign:
    begin
      Result := CalcValue2(opMul,Variable,Value);
    end;
    opDivAssign:
    begin
      Result := CalcValue2(opDiv,Variable,Value);
    end;
    opAddAssign:
    begin
      Result := CalcValue2(opAdd,Variable,Value);
    end;
    opSubAssign:
    begin
      Result := CalcValue2(opSub,Variable,Value);
    end;
    opModAssign:
    begin
      Result := CalcValue2(opMod,Variable,Value);
    end;
    opBitLeftAssign:
    begin
      Result := CalcValue2(opBitLeft,Variable,Value);
    end;
    opBitRightAssign:
    begin
      Result := CalcValue2(opBitRight,Variable,Value);
    end;
    opBitRightZeroAssign:
    begin
      Result := CalcValue2(opBitRightZero,Variable,Value);
    end;
    opBitAndAssign:
    begin
      Result := CalcValue2(opBitAnd,Variable,Value);
    end;
    opBitXorAssign:
    begin
      Result := CalcValue2(opBitXor,Variable,Value);
    end;
    opBitOrAssign:
    begin
      Result := CalcValue2(opBitOr,Variable,Value);
    end;
  end;

end;

function CompareValue(Code: TJOPCode; const L,R: TJValue): TJValue;
//比較式を作成
  function IsEqual: Boolean;
  begin
    if IsInteger(@L) and IsInteger(@R) then
      Result := L.vInteger = R.vInteger
    else if IsDouble(@L) and IsDouble(@R) then
      Result := L.vDouble = R.vDouble
    else if IsUndefined(@L) and IsUndefined(@R) then
      Result := True
    else if IsNull(@L) and IsNull(@R) then
      Result := True
    else if IsString(@L) and IsString(@R) then
      Result := L.vString = R.vString
    else if IsObject(@L) and IsObject(@R) then
      Result := L.vObject = R.vObject
    else if IsBool(@L) and IsBool(@R) then
      Result := L.vBool = R.vBool
    else if IsFunction(@L) and IsFunction(@R) then
      Result := EqualFunction(@L,@R)
    else if IsInfinity(@L) and IsInfinity(@R) then
      Result := L.vBool = R.vBool
    else if IsDispatch(@L) and IsDispatch(@R) then
      Result := L.vDispatch = R.vDispatch
    else
      Result := False;
  end;

begin
  Result := BuildBool(False);
  case Code of
    //論理比較
    opLogicalOr,opLogicalOr2: Result := BuildBool(AsBool(@L) or AsBool(@R));

    opLogicalAnd,opLogicalAnd2: Result := BuildBool(AsBool(@L) and AsBool(@R));

    opLogicalNot: Result := BuildBool(not AsBool(@L));

    opLS,opLSEQ,opGT,opGTEQ:
    begin
      //まず数値で比較
      if IsInteger(@L) and IsInteger(@R) then
      begin
        case Code of //整数で比較
          opLS: Result := BuildBool(L.vInteger < R.vInteger);
          opLSEQ: Result := BuildBool(L.vInteger <= R.vInteger);
          opGT: Result := BuildBool(L.vInteger > R.vInteger);
          opGTEQ: Result := BuildBool(L.vInteger >= R.vInteger);
        end;
      end
      else if IsDouble(@L) and IsDouble(@R) then
      begin
        case Code of //浮動で比較
          opLS: Result := BuildBool(L.vDouble < R.vDouble);
          opLSEQ: Result := BuildBool(L.vDouble <= R.vDouble);
          opGT: Result := BuildBool(L.vDouble > R.vDouble);
          opGTEQ: Result := BuildBool(L.vDouble >= R.vDouble);
        end;
      end
      else if (TryAsNumber(@L) or (L.ValueType = vtInfinity)) and
              (TryAsNumber(@R) or (R.ValueType = vtInfinity)) then
      begin
        if L.ValueType = vtInfinity then
        begin
          if R.ValueType = vtInfinity then
          begin
            case Code of //どっちもInfinity
              opLS: Result := BuildBool(L.vBool and not R.vBool);
              opLSEQ: Result := BuildBool(L.vBool);
              opGT: Result := BuildBool(not L.vBool and R.vBool);
              opGTEQ: Result := BuildBool(R.vBool);
            end;
          end
          else begin
            case Code of //Infinityと数値
              opLS,opLSEQ: Result := BuildBool(L.vBool);
              opGT,opGTEQ: Result := BuildBool(not L.vBool);
            end;
          end;
        end
        else if R.ValueType = vtInfinity then
        begin
          case Code of //数値とInfinity
            opLS,opLSEQ: Result := BuildBool(not R.vBool);
            opGT,opGTEQ: Result := BuildBool(R.vBool);
          end;
        end
        else begin
          case Code of //浮動で比較(両方数値化できる)
            opLS: Result := BuildBool(AsDouble(@L) < AsDouble(@R));
            opLSEQ: Result := BuildBool(AsDouble(@L) <= AsDouble(@R));
            opGT: Result := BuildBool(AsDouble(@L) > AsDouble(@R));
            opGTEQ: Result := BuildBool(AsDouble(@L) >= AsDouble(@R));
          end;
        end;
      end
      else if (L.ValueType <> vtNaN) and (R.ValueType <> vtNaN) then
      begin
        case Code of //文字列にして比較
          opLS: Result := BuildBool(AsString(@L) < AsString(@R));
          opLSEQ: Result := BuildBool(AsString(@L) <= AsString(@R));
          opGT: Result := BuildBool(AsString(@L) > AsString(@R));
          opGTEQ: Result := BuildBool(AsString(@L) >= AsString(@R));
        end;
      end
      //NaNを含む比較は常にfalse
      {else begin
        raise EJThrow.Create(E_TYPE,'cannot compare "NaN"');
      end};
    end;
    opEQEQEQ,opNEEQEQ:
    begin
      if EqualType(@L,@R) then
      begin
        case Code of
          opEQEQEQ: Result := BuildBool(IsEqual);
          opNeEQEQ: Result := BuildBool(not IsEqual);
        end;
      end
    end;
    opEQ,opNE:
    begin
      if EqualType(@L,@R) then
      begin
        case Code of
          opEQ: Result := BuildBool(IsEqual);
          opNe: Result := BuildBool(not IsEqual);
        end;
      end
      else if IsString(@L) and IsString(@R) then
      begin
        case Code of
          opEQ: Result := BuildBool(AsString(@L) = AsString(@R));
          opNE: Result := BuildBool(AsString(@L) <> AsString(@R));
        end;
      end
      else if TryAsNumber(@L) and TryAsNumber(@R) then
      begin
        case Code of
          opEQ: Result := BuildBool(AsDouble(@L) = AsDouble(@R));
          opNE: Result := BuildBool(AsDouble(@L) <> AsDouble(@R));
        end;
      end
      else if (IsNull(@L) and IsUndefined(@R)) or
              (IsNull(@R) and IsUndefined(@L)) then
      begin
        case Code of
          opEQ: Result := BuildBool(True);
          opNe: Result := BuildBool(False);
        end;
      end
      else if IsNaN(@L) or IsNaN(@R) then
      begin
        case Code of
          opEQ: Result := BuildBool(False);
          opNe: Result := BuildBool(True);
        end;
      end
      else begin
        //文字列にして比較
        case Code of
          opEQ: Result := BuildBool(AsString(@L) = AsString(@R));
          opNE: Result := BuildBool(AsString(@L) <> AsString(@R));
        end;
      end;
    end;
  end;//case
end;

procedure GetDefaultProperties(Obj: TObject; PropNames: TStrings; Event: Boolean);
//property名を調べる
//プロパティの値を返す
var
  Count, i: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
begin
  PropNames.Clear;
  // プロパティの数を取得
  Count := GetTypeData(Obj.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      // 全てのプロパティ情報を取得
      GetPropInfos(Obj.ClassInfo, PropList);
      // それぞれのプロパティ名を一つずつ調べる
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        if not Event then
        begin
          if PropInfo^.PropType^.Kind <> tkMethod then
            PropNames.Add(PropInfo^.Name);
        end
        else begin
          if PropInfo^.PropType^.Kind = tkMethod then
            PropNames.Add(PropInfo^.Name);
        end;
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
end;

function GetDefaultProperty(Obj: TObject; const Prop: String;
  var Value: TJValue; Engine: TJBaseEngine): Boolean;
//プロパティの値を返す
var
  Count, i: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  o: TObject;
  //jo: TJObject;
  //setval: TIntegerSet;
  enum: Integer;
  vcl: TJVCLPersistent;
begin
  Result := False;
  // プロパティの数を取得
  Count := GetTypeData(Obj.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      // 全てのプロパティ情報を取得
      GetPropInfos(Obj.ClassInfo, PropList);
      // それぞれのプロパティ名を一つずつ調べる
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        //名前が一致したら(文字ケースを無視する)
        if AnsiSameText(PropInfo^.Name,Prop) then
        begin
          //アクセスメソッドが無い場合は例外
          if not Assigned(PropInfo^.GetProc) then
            Break;

          case PropInfo^.PropType^.Kind of
            //整数
            tkInteger,tkChar:
            begin
              Value := BuildInteger(GetOrdProp(Obj,PropInfo));
              Result := True;
            end;
            //列挙型 または boolean
            tkEnumeration:
            begin
              enum := GetOrdProp(Obj,PropInfo);
              Result := EnumToValue(PropInfo^.PropType^,Value,Enum);
              //true falseを調べる
              if Result then
              begin
                if AnsiSameText(Value.vString,'True') then
                  Value := BuildBool(True)
                else if AnsiSameText(Value.vString,'False') then
                  Value := BuildBool(False);
              end;
            end;
            //集合型
            tkSet:
            begin
              {Integer(setval) := GetOrdProp(Obj,PropInfo);
              //objectを作成して変換
              jo := TJObject.Create(Factory,nil);
              SetToJObject(PropInfo^.PropType^,jo,setval);
              Value := BuildObject(jo);}
              //文字列にして返す
              Value := BuildString(GetSetProp(Obj,Prop,False));
              Result := True;
            end;

            //文字列
            tkString,tkLString,tkWString:
            begin
              Value := BuildString(GetStrProp(Obj,PropInfo));
              Result := True;
            end;
            //object
            tkClass:
            begin
              o := GetObjectProp(Obj,PropInfo);
              //nilの場合もある
              if not Assigned(o) then
                Value := BuildNull
              else if o is TJObject then
                Value := BuildObject(TJObject(o))
              //TPersistentをセット
              else if o is TPersistent then
              begin
{$IFNDEF NO_VCL}
                //型変換する
                vcl := VCLCaster.Cast(o as TPersistent,Engine);
{$ELSE}
                //型変換しない
                vcl := TJVCLPersistent.Create(Engine);
                vcl.RegistVCL(o as TPersistent,False);
{$ENDIF}
                Value := BuildObject(vcl);
              end
              else begin
                Result := False;
                Break;
              end;

              Result := True;
            end;
            //double
            tkFloat:
            begin
              Value := BuildDouble(GetFloatProp(Obj,PropInfo));
              Result := True;
            end;

            tkInterface:
            begin
              Value := BuildDispatch(IDispatch(GetOrdProp(Obj,PropInfo)));
              Result := True;
            end;
{$IFNDEF NO_ACTIVEX}
            tkVariant:
            begin
              Value := VariantToValue(GetVariantProp(Obj,PropInfo),nil);
              Result := True;
            end;
{$ENDIF}
            tkInt64:
            begin
              Value := BuildDouble(GetInt64Prop(Obj,PropInfo));
              Result := True;
            end;
            //イベント
            tkMethod:
            begin
              Value := BuildEvent(GetMethodProp(Obj,PropInfo));
              Result := True;
            end;

            tkUnknown,
            //tkChar,
            tkWChar,
            tkArray,
            tkRecord,
            tkDynArray:
            begin

            end;
          end;
          //終わり
          Break;
        end;
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;

end;

function SetDefaultProperty(Obj: TObject; const Prop: String; Value: TJValue;
  ValueTypeInfo: PTypeInfo): Boolean;
//プロパティに値をセットする
var
  Count, i: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  o: TObject;
  i64: Int64;
  enum: Integer;
  setstr: String;
  pd: PTypeData;
  meth: TMethod;
begin
  Result := False;
  // プロパティの数を取得
  Count := GetTypeData(Obj.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      // 全てのプロパティ情報を取得
      GetPropInfos(Obj.ClassInfo, PropList);
      // それぞれのプロパティ名を一つずつ調べる
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        //名前が一致したら
        if AnsiSameText(PropInfo^.Name,Prop) then
        begin
          //アクセスメソッドが無い場合は終り
          if not Assigned(PropInfo^.SetProc) then
            Break;

          case PropInfo^.PropType^.Kind of
            //整数
            tkInteger:
            begin
              SetOrdProp(Obj,PropInfo,AsInteger(@Value));
              Result := True;
            end;
            //文字型
            tkChar:
            begin
              SetOrdProp(Obj,PropInfo,Ord(AsChar(@Value)));
              Result := True;
            end;
            //列挙型 または boolean
            tkEnumeration:
            begin
              if ValueToEnum(PropInfo^.PropType^,Value,enum) then
              begin
                SetOrdProp(Obj,PropInfo,enum);
                Result := True;
              end;
            end;
            //集合型
            tkSet:
            begin
              //文字ならば
              if IsString(@Value) then
                setstr := AsString(@Value)
              else if IsObject(@Value) then
                setstr := JObjectToSetStr(PropInfo^.PropType^,Value.vObject);

              if setstr <> '' then
              begin
                SetSetProp(Obj,PropInfo,setstr);
                Result := True;
              end
            end;

            //文字列
            tkString,tkLString,tkWString:
            begin
              SetStrProp(Obj,PropInfo,AsString(@Value));
              Result := True;
            end;
            //object
            tkClass:
            begin
              //型を得る
              pd := GetTypeData(PropInfo^.PropType^);
              //jobject
              if IsObject(@Value) and pd.ClassType.InheritsFrom(TJObject) then
              begin
                o := Value.vObject;
                SetObjectProp(Obj,PropInfo,o);
                Result := True;
              end
              //TPersistentをセット
              else if IsVCLObject(@Value) and
                      (Value.vObject as TJVCLPersistent).IsVCL then
              begin
                o := (Value.vObject as TJVCLPersistent).FVCL;
                //型チェックしてセット
                if o is pd.ClassType then
                begin
                  SetObjectProp(Obj,PropInfo,o);
                  Result := True;
                end;
              end
              //nullの場合もある
              else if IsNull(@Value) then
              begin
                SetObjectProp(Obj,PropInfo,nil);
                Result := True;
              end
              else
                Break;
            end;
            //double
            tkFloat:
            begin
              SetFloatProp(Obj,PropInfo,AsDouble(@Value));
              Result := True;
            end;
            //IDispatch
            tkInterface:
            begin
              if IsDispatch(@Value) then
              begin
                SetOrdProp(Obj,PropInfo,Integer(AsDispatch(@Value)));
                Result := True;
              end
{$IFNDEF NO_ACTIVEX}
              else if IsObject(@Value) and (Value.vObject is TJActiveXObject) then
              begin
                SetOrdProp(Obj,PropInfo,
                  Integer((Value.vObject as TJActiveXObject).Disp));
                Result := True;
              end
{$ENDIF}
              else
                Break;
            end;
{$IFNDEF NO_ACTIVEX}
            tkVariant:
            begin
              SetVariantProp(Obj,PropInfo,ValueToVariant(Value));
              Result := True;
            end;
{$ENDIF}
            tkInt64:
            begin
              i64 := Trunc(AsDouble(@Value));
              SetInt64Prop(Obj,PropInfo,i64);
              Result := True;
            end;
            //イベント
            tkMethod:
            begin
              //型チェック
              if IsEvent(@Value) and (PropInfo^.PropType^ = ValueTypeInfo) then
              begin
                SetMethodProp(Obj,PropInfo,Value.vEvent);
                Result := True;
              end
              else if IsNull(@Value) then
              begin
                meth.Code := nil;
                meth.Data := nil;
                SetMethodProp(Obj,PropInfo,meth);
                Result := True;
              end;
            end;

            tkUnknown,
            //tkChar,
            tkWChar,
            tkArray,
            tkRecord,
            tkDynArray:
            begin

            end;
          end;
          //終わり
          Break;
        end;
      end;

    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;

end;

procedure SetDefaultMethodNil(Obj: TObject);
//すべてのイベントを無効にする
var
  Count, i: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  method: TMethod;
begin
  // プロパティの数を取得
  Count := GetTypeData(Obj.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    method.Code := nil;
    method.Data := nil;

    GetMem(PropList, Count * SizeOf(Pointer));
    try try
      // 全てのプロパティ情報を取得
      GetPropInfos(Obj.ClassInfo, PropList);
      // それぞれのプロパティ名を一つずつ調べる
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        //アクセスメソッドが無い場合は終り
        if not Assigned(PropInfo^.SetProc) then
          Break;

        if PropInfo^.PropType^.Kind = tkMethod then
          SetMethodProp(Obj,PropInfo,method);
      end;

    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;

    except
    end;
  end;
end;

type
  TMethodTableEntry = packed record  // 可変長レコード
    Size: Word;        // エントリの大きさ
    Address: Pointer;  // メソッドのエントリポイント
    Name: ShortString; // メソッド名
   // その他
  end;

  PMethodTableEntry = ^TMethodTableEntry;

  TMethodTable = packed record
    MethodCount: Word;             // メソッド数
    FirstEntry: TMethodTableEntry; //可変長
    // 2番目、3番目
  end;

  PMethodTable = ^TMethodTable;

  PPointer = ^Pointer;

procedure EnumMethodNames(Obj: TObject; List: TStrings);
var ClassRef: TClass;         //クラス参照=VMTポインタ
    pTable: PMethodTable;     //メソッドテーブルへのポインタ
    pEntry: PMethodTableEntry;//メソッドテーブルエントリへのポインタ
    i: Integer;
begin
  List.Clear;

  ClassRef := Obj.ClassType; // クラス参照を得る

  while ClassRef <> Nil do begin // 親クラスが無くなるまで
    // メソッドテーブルを得る
    pTable := PPointer(LongInt(ClassRef) + vmtMethodTable)^;

    if pTable <> Nil then begin // テーブルが有るとは限らない
      //テーブルの最初のエントリを得る
      pEntry := @pTable.FirstEntry;
      // クラスの全メソッド名の取得
      for i := 1 to pTable^.MethodCount do
      begin
        List.Add(pEntry^.Name);
        // 次のエントリにポインタをずらす。
        pEntry := PMethodTableEntry(LongInt(pEntry) + pEntry^.Size);
      end;
    end;

    // 親クラスへ移動
    ClassRef := ClassRef.ClassParent;
  end;
end;

procedure EnumSetNames(Info: PTypeInfo; List: TStrings);
//集合型の名前を得る
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
begin
  List.Clear;
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      for i := pd.MinValue to pd.MaxValue do
        List.Add(GetEnumName(CompType,i));
    end;
  end;
end;

procedure SetToJObject(Info: PTypeInfo; Obj: TJObject; const Value);
//集合型をobjectにセットする
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
  pi: PInteger;
begin
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      pi := @Value;
      for i := pd.MinValue to pd.MaxValue do
        Obj.RegistProperty(
          GetEnumName(CompType,i),
          BuildBool((pi^ and (1 shl i)) <> 0));
    end;
  end;
end;

procedure JObjectToSet(Info: PTypeInfo; Obj: TJObject; var RetValue);
//ojectから集合型へ
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
  pdw: PDWORD;
  pw: PWORD;
  pb: PByte;
  EnumName: string;
  ENumValue: Integer;
  v: TJValue;
begin
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      pdw := @RetValue;
      pw  := @RetValue;
      pb  := @RetValue;

      case pd.OrdType of
        otSByte, otUByte: pb^ := 0;
        otSWord, otUWord: pw^ := 0;
        otSLong, otULong: pdw^ := 0;
      end;

      for i := pd.MinValue to pd.MaxValue do
      begin
        EnumName := GetEnumName(CompType,i);
        //objが所有していてtrueならば
        if Obj.FMembers.GetValue(EnumName,v) and AsBool(@v) then
        begin
          EnumValue := GetEnumValue(CompType,EnumName);
          //セット
          if EnumValue > -1 then
            case pd.OrdType of
              otSByte, otUByte: pb^ := pb^ or (1 shl EnumValue);
              otSWord, otUWord: pw^ := pw^ or (1 shl EnumValue);
              otSLong, otULong: pdw^ := pdw^ or (1 shl EnumValue);
            end;
        end;
      end;

    end;
  end;
end;

function JObjectToSetStr(Info: PTypeInfo; Obj: TJObject): String;
//ojectから集合型の文字列へ
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
  EnumName: string;
  v: TJValue;
begin
  Result := '';
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      for i := pd.MinValue to pd.MaxValue do
      begin
        EnumName := GetEnumName(CompType,i);
        //objが所有していてtrueならば
        if Obj.FMembers.GetValue(EnumName,v) and AsBool(@v) then
          if Result = '' then
            Result := EnumName
          else
            Result := Result + ',' + EnumName;
      end;
    end;
  end;

  //Result := '[' + Result + ']';
end;

function SetToStr(Info: PTypeInfo; const Value): string;
//集合から文字へ
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
  pi: PInteger;
begin
  Result := '';
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      pi := @Value;
      for i := pd.MinValue to pd.MaxValue do
        if (pi^ and (1 shl i)) <> 0 then
          if Result = '' then
            Result := GetEnumName(CompType, i)
          else
            Result := Result + ',' + GetEnumName(CompType, i);
    end;
  end;

  //Result := '[' + Result + ']';
end;

procedure StrToSet(Info: PTypeInfo; const S: String; var RetValue);
var
  pd: PTypeData;
  CompType: PTypeInfo;
  i: Integer;
  pdw: PDWORD;
  pw: PWORD;
  pb: PByte;
  p: PChar;
  EnumName: string;
  ENumValue: Integer;
begin
  pd := GetTypeData(Info);
  if Assigned(pd) and Assigned(pd.CompType^) then
  begin
    CompType := pd.CompType^;
    pd := GetTypeData(CompType);
    if Assigned(pd) then
    begin
      pdw := @RetValue;
      pw  := @RetValue;
      pb  := @RetValue;

      case pd.OrdType of
        otSByte, otUByte: pb^ := 0;
        otSWord, otUWord: pw^ := 0;
        otSLong, otULong: pdw^ := 0;
      end;

      p := PChar(S);

      // '[' と ' ' をスキップ
      while p^ in ['[',' '] do
        Inc(p);

      // ','  ' ' #0 ']' までを要素名として取り出す
      i := 0;
      while not (p[i] in [',', ' ', #0,']']) do
        Inc(i);

      SetString(EnumName, p, i);
      // 次の語の先頭までポインタを進める
      while p[i] in [',', ' ',']'] do
        Inc(i);

      Inc(p, i);

      while EnumName <> '' do
      begin
        EnumValue := GetEnumValue(CompType, EnumName);
        if EnumValue > -1 then
        begin
          case pd.OrdType of
            otSByte, otUByte: pb^ := pb^ or (1 shl EnumValue);
            otSWord, otUWord: pw^ := pw^ or (1 shl EnumValue);
            otSLong, otULong: pdw^ := pdw^ or (1 shl EnumValue);
          end;
        end;
        // ','  ' ' #0 ']' までを要素名として取り出す
        i := 0;
        while not (p[i] in [',', ' ', #0,']']) do
          Inc(i);

        SetString(EnumName, p, i);
        // 次の語の先頭までポインタを進める
        while p[i] in [',', ' ',']'] do
          Inc(i);

        Inc(p, i);
      end;

    end;
  end;
end;

function ValueToEnum(Info: PTypeInfo; var Value: TJValue;
  var Enum: Integer): Boolean;
//列挙型を変換する
begin
  //数値にできない場合
  if not TryAsNumber(@Value) then
    Enum := GetEnumValue(Info,AsString(@Value))
  else
    Enum := AsInteger(@Value);

  Result := (Enum > -1);
end;

function EnumToValue(Info: PTypeInfo; var Value: TJValue;
  Enum: Integer): Boolean;
var
  s: String;
begin
  s := GetEnumName(Info,Enum);
  Result := (s <> '');
  if Result then
    Value := BuildString(s)
  else
    EmptyValue(Value);
end;


{ TJHash }

procedure TJHash.ClearValue(Target, Ignore: TJValueTypeSet);
//種類を選んでクリア
var
  i: Integer;
  sl: TStringList;
  v: TJValue;
begin
  //すべて消す
  if (Target = []) and (Ignore = []) then
    Clear
  else begin
    //選んで消す
    sl := KeyList;
    for i := sl.Count - 1 downto 0 do
      if GetValue(sl[i],v) then
      begin
        if Target <> []  then
        begin
          //targetならば消す
          if v.ValueType in Target then
            Remove(sl[i]);
        end
        else begin
          //ignoreでないならば消す
          if not (v.ValueType in Ignore) then
            Remove(sl[i]);
        end;
      end;
  end;

end;

constructor TJHash.Create(ATableSize: DWord; AIgnoreCase: Boolean);
//作成
begin
  inherited;
  FNotify := TJNotify.Create;
  FNotify.OnNotification := NotifyOnNotifycation;
  OnFreeItem := HashOnItemDispose;
end;

destructor TJHash.Destroy;
//開放する
begin
  Clear;
  FreeAndNil(FNotify);
  inherited;
end;

procedure TJHash.GetKeyList(List: TStrings; Need,
  Ignore: TJValueTypeSet);
var
  i: Integer;
  v: TJValue;
  sl: TStringList;
begin
  List.Clear;

  sl := KeyList;
  //加える
  for i := 0 to sl.Count - 1 do
    if GetValue(sl[i],v) then
    begin
      if Need <> [] then
      begin
        //Needだったら加える
        if v.ValueType in Need then
          List.Add(sl[i]);
      end
      else {if Ignore <> [] then}
      begin
        //Ignoreでないならば加える
        if not (v.ValueType in Ignore) then
          List.Add(sl[i]);
      end;
    end;
end;

function TJHash.GetValue(Key: String; var Value: TJValue): Boolean;
var
  p: PJValue;
begin
  p := GetValuePointer(Key);
  if Assigned(p) then
  begin
    Result := True;
    Value := p^;
  end
  else begin
    Result := False;
    EmptyValue(Value);
  end;
end;

procedure TJHash.HashOnItemDispose(Sender: TObject; P: PHashItem);
//valueを解放する
var
  value: PJValue;
begin
  value := P^.vPointer;
  if IsObject(value) then
  begin
    //notifycationを解除
    value^.vObject.RemoveFreeNotification(FNotify);
    FNotify.RemoveFreeNotification(value^.vObject);
    //参照カウントを減らす
    value^.vObject.DecRef;
  end;

  Dispose(value);
  P^.vPointer := nil;
end;

procedure TJHash.NotifyOnNotifycation(Sender: TObject);
//objectの削除イベント
var
  i,ii: Integer;
  pv: PJValue;
begin
  //この時点でsenderのnotifyの登録は解除されてる
  //hashから削除しないで、無効にするだけ
  for i := Length(FTable) - 1 downto 0 do
  begin
    if Assigned(FTable[i]) then
    begin
      //pointerが一致すれば削除
      for ii := FTable[i].Count - 1 downto 0 do
      begin
        pv := FTable[i][ii].vPointer;
        //無効にする
        if IsObject(pv) and (pv^.vObject = Sender) then
          EmptyValue(pv^);
      end;
    end;
  end;
end;

procedure TJHash.SetValue(Key: String; Value: TJValue);
//新しく作ってセットする
var
  p: PJValue;
begin
  if IsObject(@Value) then
  begin
    //notifycationをセット
    Value.vObject.FreeNotification(FNotify);
    //参照カウントを増やす
    Value.vObject.IncRef;
  end;

  New(p);
  p^ := Value;
  SetValuePointer(Key,p);
end;

{ TJObject }

procedure TJObject.RegistName(AName: String);
begin
  FName := AName;
end;

procedure TJObject.ClearMembers;
//データをクリア
begin
  FMembers.ClearValue([],[]);
end;

constructor TJObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//object作成
var
  fact: TJObjectFactory;
begin
  inherited Create;
  //参照カウントを一時的に増やす
  FRefCount := 1;

  FEngine := AEngine;

  FMembers := TJHash.Create(HASH_20);
  FDefaultProperties := TStringList.Create;
  FDefaultProperties.Sorted := True;
  FDefaultProperties.Duplicates := dupIgnore;
  GetDefaultProperties(Self,FDefaultProperties);

  RegistName('Object');
  RegistMethods;
  RegistProperties;

  if Assigned(AEngine) and RegisteringFactory then
  begin
    fact := TJEngine(AEngine).Factory;
    //イベントを起こす
    if Assigned(fact.FOnNewObject) then
      fact.FOnNewObject(fact,Self);
    //factoryへ加える
    fact.Add(Self);
  end;
end;

destructor TJObject.Destroy;
//破棄
begin
  FreeAndNil(FEvents);
  FreeAndNil(FDefaultProperties);
  //自己参照を無効にするため
  //FMembersを開放するより先にnotificationを起こす
  inherited;
  FreeAndNil(FMembers);
end;

function TJObject.GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
//メンバを得る
begin
  if not GetValueImpl(S,Result,Param) then
  begin
    //配列
    if ArrayStyle then
      raise EJThrow.Create(E_KEY,S)
    else //メンバ
      raise EJThrow.Create(E_NAME,S);
  end;
end;

procedure TJObject.GetPropertyList(List: TStringList);
//全てのpropertyを得る
begin
  GetKeyList(List,[],[vtFunction]);
  List.AddStrings(FDefaultProperties);
end;

function TJObject.HasKey(S: String): Boolean;
//memberを持ってる？
var
  v: TJValue;
begin
  if HasDefaultProperty(S) then
    Result := True
  else begin
    Result := FMembers.GetValue(S,v);
  end;
end;

function TJObject.HasDefaultProperty(Prop: String): Boolean;
//propertyを持っているかチェック
var
  i: Integer;
begin
  Result := FDefaultProperties.Find(Prop,i);
end;

procedure TJObject.RegistMethod(MethodName: String; Method: TJMethod);
//関数を登録する
var
  f: IJFunction;
begin
  EmptyFunction(f);
  f.Symbol := MethodName;
  f.FuncType := ftMethod;
  f.vMethod := Method;
  f.MethodOwner := Self;
  FMembers.SetValue(MethodName,BuildFunction(f));
end;

procedure TJObject.SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil);
//メンバをセット
begin
  //falseでも無条件で登録
  if not SetValueImpl(S,Value,Param) then
    FMembers.SetValue(S,Value);
end;

procedure TJObject.RegistProperty(PropName: String; Value: TJValue);
//proeprtyを登録する
begin
  FMembers.SetValue(PropName,Value);
end;

function TJObject.DoHasKey(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(HasKey(AsString(@v)));
  end;
end;

function TJObject.DoToString(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(ToString(@v));
  end
  else
    Result := BuildString(ToString);
end;

function TJObject.ToString(Value: PJValue): String;
begin
  Result := '[object ' + Name + ']';
end;

procedure TJObject.GetMethodList(List: TStringList);
//全てのmethodを得る
begin
  GetKeyList(List,[vtFunction],[]);
end;

function TJObject.DoGetKeys(Param: TJValueList): TJValue;
var
  sl: TStringList;
  i: Integer;
  ary: TJArrayObject;
begin
  sl := TStringList.Create;
  try
    GetKeyList(sl,[],[]);
    sl.AddStrings(FDefaultProperties);
    //Result := BuildString(Trim(sl.Text));
    //配列に入れて返す
    ary := TJArrayObject.Create(FEngine);
    Result := BuildObject(ary);
    for i := 0 to sl.Count - 1 do
      ary.Items.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

function TJObject.DoGetMethods(Param: TJValueList): TJValue;
var
  sl: TStringList;
  i: Integer;
  ary: TJArrayObject;
begin
  sl := TStringList.Create;
  try
    GetMethodList(sl);
    //Result := BuildString(Trim(sl.Text));
    //配列に入れて返す
    ary := TJArrayObject.Create(FEngine);
    Result := BuildObject(ary);
    for i := 0 to sl.Count - 1 do
      ary.Items.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

function TJObject.DoGetProperties(Param: TJValueList): TJValue;
var
  sl: TStringList;
  i: Integer;
  ary: TJArrayObject;
begin
  sl := TStringList.Create;
  try
    GetPropertyList(sl);
    //Result := BuildString(Trim(sl.Text));
    //配列に入れて返す
    ary := TJArrayObject.Create(FEngine);
    Result := BuildObject(ary);
    for i := 0 to sl.Count - 1 do
      ary.Items.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TJObject.RegistMethods;
//メソッドを登録する
begin
  RegistMethod('hasKey',DoHasKey);
  RegistMethod('hasOwnProperty',DoHasKey);
  RegistMethod('removeKey',DoRemoveKey);
  RegistMethod('toString',DoToString);
  RegistMethod('getKeys',DoGetKeys);
  RegistMethod('getProperties',DoGetProperties);
  RegistMethod('getMethods',DoGetMethods);
  RegistMethod('valueOf',DoValueOf);
  RegistMethod('getEvents',DoGetEvents);
end;

procedure TJObject.ClearProperties;
//propertyだけ消す(関数を消さない)
begin
  //消すのは今のpropertyだけ
  FMembers.ClearValue([],[vtFunction]);
end;

function TJObject.Equal(Obj: TJObject): Boolean;
begin
  Result := Obj = Self;
end;

function TJObject.ToNumber: Double;
begin
  Result := 0;
end;

function TJObject.ToBool: Boolean;
begin
  Result := True;
end;

function TJObject.ToChar: Char;
begin
  Result := #0;
end;

function TJObject.RemoveKey(S: String): Boolean;
//keyを削除する
begin
  Result := FMembers.Remove(S);
end;

function TJObject.DoRemoveKey(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(RemoveKey(AsString(@v)));
  end;
end;

procedure TJObject.AfterConstruction;
//コンストラクタ後
begin
  inherited;
  //参照カウントを減らす
  FRefCount := 0;
end;

function TJObject.DecRef: Integer;
begin
  //0になったら解放する
  Dec(FRefCount);
  Result := FRefCount;

  if FRefCount <= 0 then
    Free;
end;

function TJObject.IncRef: Integer;
begin
  Inc(FRefCount);
  REsult := FRefCount;
end;

function TJObject.ValueOf: TJValue;
begin
  Result := BuildObject(Self);
end;

function TJObject.DoValueOf(Param: TJValueList): TJValue;
begin
  Result := ValueOf;
end;

procedure TJObject.GetKeyList(List: TStringList; Need,
  Ignore: TJValueTypeSet);
begin
  FMembers.GetKeyList(List,Need,Ignore);
  //List.AddStrings(FDefaultProperties);
end;

procedure TJObject.Notification(AObject: TJNotify);
//所有objectの終了通知
begin
  inherited;
end;

class function TJObject.IsMakeGlobalInstance: Boolean;
//GlobalInstanceをエンジンに作成するかどうか
begin
  Result := True;
end;

function TJObject.CallEvent(Prefix,EventName: String;
  Param: TJValueList): TJValue;
//イベントを呼ぶ
var
  v: TJValue;
  eng: TJEngine;
begin
  EmptyValue(Result);
  if not Assigned(FEngine) then
    Exit;

  eng := FEngine as TJEngine;

  //prefixがある場合はスクリプト内の関数
  if (Prefix <> '') then
  begin
    if eng.GetVariable(Prefix + EventName,v) then
      Result := eng.CallEvent(v,Param,Self);
  end
  else begin
    //プロパティから探す
    v := GetValue(EventName,True);
    Result := eng.CallEvent(v,Param,Self);
  end;
end;

function TJObject.IsCallEvent(EventName: String): Boolean;
//可能かチェック
var
  v: TJValue;
begin
  Result := Assigned(FEngine) and (FEngine as TJEngine).AllowEvent;
  if Result and (EventName <> '') then
    Result := GetValueImpl(EventName,v);
end;

function TJObject.GetValueImpl(S: String;
  var RetVal: TJValue; Param: TJValueList): Boolean;
var
  fact: TJObjectFactory;
begin
  if GetDefaultProperty(Self,S,RetVal,FEngine) then
    Result := True
  else if FMembers.GetValue(S,RetVal) then
    Result := True
  else if (not (Self is TJPrototypeObject)) and Assigned(FEngine) then
  begin
    //prototype
    fact := TJEngine(FEngine).Factory;
    Result := fact.GetPrototype(Name).GetValueImpl(S,RetVal,Param);
  end
  else
   Result := False;
end;

function TJObject.SetValueImpl(S: String;
  var Value: TJValue; Param: TJValueList): Boolean;
begin
  Result := False;
  if SetDefaultProperty(Self,S,Value) then
    Result := True
  else begin
    if FMembers.HasKey(S) then
    begin
      FMembers.SetValue(S,Value);
      Result := True;
    end;
  end;
end;

procedure TJObject.Clear;
//削除する
begin
  ClearValue([],[]);
  RegistMethods;
  Registproperties;
end;

procedure TJObject.ClearValue(Target, Ignore: TJValueTypeSet);
begin
  FMembers.ClearValue(Target,Ignore);
end;

procedure TJObject.Registproperties;
begin
//ここを継承してメンバプロパティを登録する
end;

function TJObject.GetCount: Integer;
begin
  Result := 0;
end;

function TJObject.GetItem(Index: Integer): TJValue;
begin
  EmptyValue(Result);
end;

class function TJObject.IsArray: Boolean;
begin
  //for..in文で要素を変数に入れる場合はtrueにする
  Result := False;
end;

procedure TJObject.GetEventList(List: TStrings);
begin
//イベント名
  if Assigned(FEvents) then
    List.Assign(FEvents)
  else
    List.Clear;
end;

function TJObject.DoGetEvents(Param: TJValueList): TJValue;
var
  sl: TStringList;
  i: Integer;
  ary: TJArrayObject;
begin
  sl := TStringList.Create;
  try
    GetEventList(sl);
    //配列に入れて返す
    ary := TJArrayObject.Create(FEngine);
    Result := BuildObject(ary);
    for i := 0 to sl.Count - 1 do
      ary.Items.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TJObject.RegistEventName(EventName: String);
begin
  if not Assigned(FEvents) then
  begin
    FEvents := TStringList.Create;
    FEvents.Sorted := True;
    FEvents.Duplicates := dupIgnore;
  end;

  FEvents.Add(EventName);
end;


{ TJVCLPersistent }

constructor TJVCLPersistent.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCL');
  //作成する
  CreateObjects;
  CreateVCL;

  RegistMethod('assign',DoAssign);
end;

procedure TJVCLPersistent.CreateObjects;
begin
  //何もしない
end;

procedure TJVCLPersistent.CreateVCL;
begin
  //ここをoverrideしてVCLを作成する
  //RegistVCL(T.Create(nil),True);
end;

destructor TJVCLPersistent.Destroy;
begin
  //VCLを破棄する
  DestroyVCL;
  inherited;
end;

procedure TJVCLPersistent.DestroyVCL;
//VCLをクリア
begin
  if not Assigned(FVCL) then
    Exit;

  //開放する
  if FCanDestroy then
    FreeAndNil(FVCL);

  FVCL := nil;
end;

procedure TJVCLPersistent.GetPropertyList(List: TStringList);
//VCL.propertyのみ
begin
  List.BeginUpdate;
  try
    List.Clear;
    List.AddStrings(FDefaultProperties);
  finally
    List.EndUpdate;
  end;
end;

function TJVCLPersistent.GetValueImpl(S: String;
  var RetVal: TJValue; Param: TJValueList): Boolean;
//値を得る
begin
  //selfを優先する
  if inherited GetValueImpl(S,RetVal) then
    Result := True
  //VCLから探す published
  else if Assigned(FVCL) and
          HasDefaultProperty(S) and
          GetDefaultProperty(FVCL,S,RetVal,FEngine) then
  begin
    Result := True;
    //objectの場合は登録する
    if IsVCLObject(@RetVal) then
      RegistProperty(S,RetVal);
  end
  else
    Result := False;
end;

function TJVCLPersistent.GetVCLClassName: String;
begin
  Result := VCLClassType.ClassName;
end;

function TJVCLPersistent.IsVCL: Boolean;
begin
  Result := Assigned(FVCL);
end;

class function TJVCLPersistent.IsMakeGlobalInstance: Boolean;
begin
  Result := False;
end;

procedure TJVCLPersistent.RegistEvents;
//イベントを登録する
{ TODO : 同名で別の型のイベントがあった場合の動作が不明 }
{ TODO : 手動で登録した方が速い }
//var
  //sl: TSTringList;
  //i: Integer;
  //v: TJValue;
  //meth: TMethod;
begin
  {if not Assigned(FVCL) then
    Exit;

  sl := TStringList.Create;
  try
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;
    //自身のイベント名を探す
    EnumMethodNames(Self,sl);
    //イベントをセットする
    for i := 0 to sl.Count - 1 do
    begin
      //TMethodを得る
      meth.Data := Self;
      meth.Code := Self.MethodAddress(sl[i]);
      v := BuildEvent(meth);
      //VCLにセットする
      SetDefaultProperty(FVCL,sl[i],v);
    end;
  finally
    sl.Free;
  end;}
end;

function TJVCLPersistent.RegistVCL(AVCL: TPersistent;
  ACanDestroy: Boolean): Boolean;
//VCLを登録
var
  sl: TStringList;
begin
  //以前を破棄
  DestroyVCL;
  //型チェック
  if not (AVCL is VCLClassType) then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;

  FCanDestroy := ACanDestroy;
  FVCL := AVCL;
  if Assigned(FVCL) then
  begin
    sl := TStringList.Create;
    try
      //propertyを登録
      GetDefaultProperties(FVCL,sl);
      FDefaultProperties.Assign(sl);
      //自分
      sl.Clear;
      GetDefaultProperties(Self,sl);
      FDefaultProperties.AddStrings(sl);
    finally
      sl.Free;
    end;
    //イベント登録
    RegistEvents;
  end;
end;

function TJVCLPersistent.SetValueImpl(S: String;
  var Value: TJValue; Param: TJValueList): Boolean;
begin
  //selfを優先する
  if inherited SetValueImpl(S,Value) then
    Result := True
  else if Assigned(FVCL) and
          HasDefaultProperty(S) and
          SetDefaultProperty(FVCL,S,Value) then
  begin
    Result := True;
  end
  else
    Result := False;
end;

class function TJVCLPersistent.VCLClassType: TClass;
begin
  Result := TPersistent;
end;

procedure TJVCLPersistent.Error(Msg: String);
begin
  raise EJThrow.Create(E_VCL,
    VCLClassType.ClassName + ' - ' + Msg);
end;

procedure TJVCLPersistent.CheckVCL(Param: TJValueList; ArgCount: Integer);
//例外を起こす
begin
  if not IsVCL then
    Error('VCL is null')
  else begin
    //引数チェック
    if ArgCount > 0 then
    begin
      if not Assigned(Param) then
        ArgsError
      else if Assigned(Param) and (Param.Count < ArgCount) then
        ArgsError;
    end;
  end;
end;

function TJVCLPersistent.DoAssign(Param: TJValueList): TJValue;
//VCLのコピー
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  if IsVCLObject(@v) and (v.vObject as TJVCLPersistent).IsVCL then
    FVCL.Assign((v.vObject as TJVCLPersistent).FVCL);
end;

procedure TJVCLPersistent.ArgsError;
begin
  Error('arguments error');
end;

{ EJReturn }

constructor EJReturn.Create(AValue: TJValue);
begin
  inherited Create('return');
  FValue := AValue;
end;

{ TJValueList }

function TJValueList.Add(Value: TJValue; IncRef: Boolean): Integer;
//加える
var
  p: PJValue;
begin
  New(p);
  p^ := Value;

  if IsObject(p) then
  begin
    //notificationをセット
    p^.vObject.FreeNotification(FNotify);
    //参照カウント
    if IncRef then
      p^.vObject.IncRef;
  end;

  Result := FItems.Add(p);
end;

procedure TJValueList.Clear;
//クリア
var
  i: Integer;
begin
  for i := FItems.Count - 1 downto 0 do
    Delete(i);

  FItems.Clear;
end;

function TJValueList.GetCount: Integer;
//カウント
begin
  Result := FItems.Count;
end;

constructor TJValueList.Create;
//作成
begin
  inherited Create;
  FItems := TListPlus.Create;
  FItems.SortType := stMerge;//stQuick;
  FNotify := TJNotify.Create;
  FNotify.OnNotification := NotifyOnNotifycation;
end;

procedure TJValueList.Delete(Index: Integer);
//削除
var
  p: PJValue;
begin
  p := FItems[Index];

  if IsObject(p) then
  begin
    //notificationを解除
    p^.vObject.RemoveFreeNotification(FNotify);
    FNotify.RemoveFreeNotification(p^.vObject);
    //参照カウント
    p^.vObject.DecRef;
  end;

  Dispose(p);
  FItems.Delete(Index);
end;

destructor TJValueList.Destroy;
//破棄
begin
  Clear;
  FreeAndNil(FNotify);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TJValueList.GetItems(Index: Integer): TJValue;
//ゲット
var
  p: PJValue;
begin
  EmptyValue(Result);
  p := FItems[Index];
  if Assigned(p) then
    Result := p^;
end;

procedure TJValueList.Insert(Index: Integer; Value: TJValue);
//挿入
var
  p: PJValue;
begin
  New(p);
  p^ := Value;
  if IsObject(p) then
  begin
    //notificationをセット
    p^.vObject.FreeNotification(FNotify);
    //参照カウント
    p^.vObject.IncRef;
  end;

  FItems.Insert(Index,p);
end;

procedure TJValueList.SetItems(Index: Integer; const Value: TJValue);
//セット
var
  p: PJValue;
begin
  //参照カウントを増やす
  if IsObject(@Value) then
    Value.vObject.IncRef;

  p := FItems[Index];

  if IsObject(p) then
  begin
    //通知を消す
    p^.vObject.RemoveFreeNotification(FNotify);
    FNotify.RemoveFreeNotification(p^.vObject);
    //以前の値の参照カウントを減らす
    p^.vObject.DecRef;
  end;
  //入れ替え
  p^ := Value;
  //通知をセット
  if IsObject(p) then
    p^.vObject.FreeNotification(FNotify);
end;

procedure TJValueList.Sort(Compare: TListSortCompareObj);
begin
  FItems.Sort(Compare);
end;

procedure TJValueList.SetCount(const Value: Integer);
var
  i,cnt: Integer;
  v: TJValue;
begin
  if Value > FItems.Count then
  begin
    //大きくする
    EmptyValue(v);
    cnt := Value - FItems.Count;
    for i := 0 to cnt - 1 do
      Add(v);
  end
  else if Value < FItems.Count then
  begin
    //小さくする
    for i := FItems.Count - 1 downto Value do
      Delete(i);
  end;
end;

function TJValueList.GetSortType: TSortType;
begin
  Result := FItems.SortType;
end;

procedure TJValueList.SetSortType(const Value: TSortType);
begin
  FItems.SortType := Value;
end;

procedure TJValueList.Assign(Source: TJValueList);
//コピー
var
  i: Integer;
begin
  Clear;
  if Assigned(Source) then
    for i := 0 to Source.Count - 1 do
      Add(Source[i]);
end;

function TJValueList.Add(Value: Boolean): Integer;
begin
  Result := Add(BuildBool(Value));
end;

function TJValueList.Add(Value: Integer): Integer;
begin
  Result := Add(BuildInteger(Value));
end;

function TJValueList.Add(Value: TJObject; IncRef: Boolean): Integer;
begin
  Result := Add(BuildObject(Value),IncRef);
end;

function TJValueList.Add(Value: Double): Integer;
begin
  Result := Add(BuildDouble(Value));
end;

function TJValueList.Add(Value: IDispatch): Integer;
begin
  Result := Add(BuildDispatch(Value));
end;

function TJValueList.Add(Value: String): Integer;
begin
  Result := Add(BuildString(Value));
end;

procedure TJValueList.NotifyOnNotifycation(Sender: TObject);
//objectの削除イベント
var
  i: Integer;
  p: PJValue;
begin
  for i := FItems.Count - 1 downto 0 do
  begin
    p := FItems[i];
    //無効にするだけ
    if IsObject(p) and (p^.vObject = Sender) then
      EmptyValue(p^);
  end;
end;

{ TJObjectFactory }

procedure TJObjectFactory.ImportObject(ObjectName: String;
  ObjectClass: TJObjectClass);
//objectclassを登録する
var
  p: PJObjectClass;
begin
  New(p);
  p^ := ObjectClass;
  FHash[ObjectName] := p;
end;

constructor TJObjectFactory.Create(AEngine: TJBaseEngine);
//作成
begin
  inherited Create;
  FEngine := AEngine;
  FHash := TPointerHashTable.Create(HASH_50);
  FHash.OnFreeItem := HashOnItemDispose;
  FItems := TJObjectList.Create;
  //prototype
  FProto := TJHash.Create(HASH_20);
end;

destructor TJObjectFactory.Destroy;
//破棄
begin
  Clear;
  FreeAndNil(FProto);
  FreeAndNil(FItems);
  FreeAndNil(FHash);
  inherited;
end;

procedure TJObjectFactory.HashOnItemDispose(Sender: TObject; P: PHashItem);
//個別itemの破棄
var
  obj: PJObjectClass;
begin
  obj := P^.vPointer;
  Dispose(obj);
  P^.vPointer := nil;
end;

function TJObjectFactory.HasObject(ObjectName: String): Boolean;
//objectがある？
begin
  Result := FHash.HasKey(ObjectName);
end;

procedure TJObjectFactory.DeleteObject(ObjectName: String);
//object classを削除する
begin
  FHash.Remove(ObjectName);
end;

procedure TJObjectFactory.Add(Obj: TJObject);
begin
  FItems.Add(Obj);
  Obj.FreeNotification(Self);
end;

procedure TJObjectFactory.Clear;
begin
  //必ずprototypeを先に開放する
  FProto.Clear;
  FItems.Clear;
end;

function TJObjectFactory.GetObjectCount: Integer;
begin
  Result := FItems.Count;
end;

function TJObjectFactory.GetObjectNameList: TStringList;
begin
  Result := FHash.KeyList;
end;

function TJObjectFactory.GetObject(ObjectName: String): PJObjectClass;
begin
  Result := FHash[ObjectName];
end;

function TJObjectFactory.GetPrototype(ObjectName: String): TJObject;
//prototypeを作成
var
  v: TJValue;
begin
  //返すのは.Prototype
  if FProto.GetValue(ObjectName,v) then
    Result := (v.vObject as TJPrototypeObject).Prototype
  else begin
    //そのまま返す
    Result := TJPrototypeObject.Create(FEngine);
    //セット
    FProto.SetValue(ObjectName,BuildObject(Result));
  end;
end;

function TJObjectFactory.SetPrototype(ObjectName: String; Obj: TJObject): Boolean;

  function CheckPrototype: Boolean;
  //無限ループをチェックする
  var
    p: TJObject;
  begin
    Result := True;
    //nullだったら終わり
    if not Assigned(Obj) then
      Exit;

    //Obj.Nameのprototypeを得る
    p := GetPrototype(Obj.Name);
    while not(p is TJPrototypeObject) do
    begin
      //無限ループになるのでエラー
      if p.Name = ObjectName then
      begin
        Result := False;
        Break;
      end
      else begin //次チェック
        p := GetPrototype(p.Name);
        Result := True;
      end;
    end;
  end;

var
  v: TJValue;
begin
  //check
  Result := CheckPrototype;
  if Result then
  begin
    //作成する
    GetPrototype(ObjectName);
    //セット
    if FProto.GetValue(ObjectName,v) then
      (v.vObject as TJPrototypeObject).Prototype := Obj;
  end;
end;

procedure TJObjectFactory.Notification(AObject: TJNotify);
//object削除イベント
begin
  inherited;
  FItems.Remove(AObject);
end;

{ EJRuntimeError }

constructor EJRuntimeError.Create(AExceptName,AErrorMsg: String;
  AValue: PJValue);
begin
  inherited Create(AExceptName);
  FExceptName := AExceptName;
  Message := AErrorMsg;
  FValue := BuildNull;
  if Assigned(AValue) then
    FValue := AValue^;
end;

{ EJExit }

constructor EJExit.Create(AStatus: Integer);
begin
  FStatus := AStatus;
end;

{ TJObjectList }

procedure TJObjectList.Clear;
//var
//  s: String;
begin
  { if Count > 0 then
   begin
     s := Format('%d個のobjectが解放されませんでした',[Count]);
     MessageBox(0,PChar(s),'DMonkey',MB_OK);
     sleep(0); //ブレークポイントのためで、特に意味はなし
   end;
  }
  //先頭から消す
  while (Count > 0) do
    TObject(Items[0]).Free;

  inherited;
end;

{ TJLocalSymbolTable }

procedure TJLocalSymbolTable.Clear;
//クリアする
begin
  //下位をクリア
  FTables.Clear;
  //thisをクリア
  SetThis(nil);
  //tempをクリア
  FTempObjects.Clear;
  //localをクリア
  FLocal.Clear;
end;

constructor TJLocalSymbolTable.Create(AParent: TJLocalSymbolTable);
//作成
begin
  inherited Create;
  FParent := AParent;
  FLocal := TJHash.Create(HASH_30);
  FTables := TObjectList.Create;
  FTempObjects := TJValueList.Create;
end;

destructor TJLocalSymbolTable.Destroy;
//破棄
begin
  Clear;
  FreeAndNil(FTempObjects);
  FreeAndNil(FTables);
  FreeAndNil(FLocal);
  inherited;
end;

function TJLocalSymbolTable.GetGlobalTable: TJGlobalSymbolTable;
//globalテーブルを得る
var
  table: TJLocalSymbolTable;
begin
  Result := nil;
  table := Self;
  while Assigned(table) do
  begin
    if table is TJGlobalSymbolTable then
    begin
      Result := table as TJGlobalSymbolTable;
      Break;
    end
    else //親へ移動
      table := table.FParent;
  end;
end;

function TJLocalSymbolTable.GetValueImpl(Caller: TJLocalSymbolTable;
  Symbol: String; var Value: TJValue): Boolean;
//値を得る実装部
begin
  //ローカルを探す
  if FLocal.GetValue(Symbol,Value) then
  begin
    Result := True;
    Exit;
  end;

  //thisを探す
  if Assigned(FThis) then
  begin
{$IFNDEF NO_ACTIVEX}
    if FThis is TJActiveXObject then
    begin
      try
        Value := FThis.GetValue(Symbol,False);
        Result := True;
        Exit;
      except
        on EJThrow do
      end;
    end else
{$ENDIF}
    if FThis.HasKey(Symbol) then
    begin
      Value := FThis.GetValue(Symbol,False);
      Result := True;
      Exit;
    end;
  end;

  //親を探す
  if Assigned(FParent) then
    Result := FParent.GetValueImpl(Self,Symbol,Value)
  else
    Result := False;
end;

function TJLocalSymbolTable.GetGlobalValue(Symbol: String;
  var Value: TJValue): Boolean;
//global tableの値を得る
begin
  Result := GetGlobalTable.GetValue(Symbol,Value);
end;

function TJLocalSymbolTable.GetThis: TJObject;
//thisを探す
begin
  Result := FThis;
  //親から探す
  if (not Assigned(Result)) and Assigned(FParent) then
    Result := FParent.GetThis
end;

function TJLocalSymbolTable.GetValue(Symbol: String;
  var Value: TJValue): Boolean;
//値を得る
begin
  Result := GetValueImpl(Self,Symbol,Value);
end;

function TJLocalSymbolTable.PushLocalTable(
  ATable: TJLocalSymbolTable; AThis: TJObject): TJLocalSymbolTable;
//localテーブルを作成
begin
  Result := TJLocalSymbolTable.Create(Self);
  FTables.Insert(0,Result);
  //値をコピー
  if Assigned(ATable) then
    Result.LocalCopy(ATable);
  //this
  Result.SetThis(AThis);
end;

procedure TJLocalSymbolTable.RegistGlobalValue(Symbol: String;
  Value: TJValue);
//グローバルに登録する
begin
  GetGlobalTable.RegistValue(Symbol,Value);
end;

procedure TJLocalSymbolTable.RegistValue(Symbol: String; Value: TJValue);
//localに登録する
begin
  FLocal.SetValue(Symbol,Value);
end;

procedure TJLocalSymbolTable.PopLocalTable;
//tableを削除
begin
  //最初を消す
  FTables.Delete(0);
end;

procedure TJLocalSymbolTable.SetValue(Symbol: String;
  Value: TJValue; RegistType: TJRegistVarType);
begin
  if not SetValueImpl(Self,Symbol,Value) then
    case RegistType of
      //無い場合はローカルに登録
      rvLocal:  RegistValue(Symbol,Value);
      //無い場合は関数staticに登録
      rvStatic: RegistStaticValue(Symbol,Value);
    else //rvGlobal:
      //無い場合はグローバルに登録
      RegistGlobalValue(Symbol,Value);
    end;
end;

function TJLocalSymbolTable.SetValueImpl(Caller: TJLocalSymbolTable;
  Symbol: String; var Value: TJValue): Boolean;
//登録されているテーブルを探して登録する
begin
  //ローカルを探す
  if FLocal.HasKey(Symbol) then
  begin
    FLocal.SetValue(Symbol,Value);
    Result := True;
    Exit;
  end;

  //thisを探す
  if Assigned(FThis) and FThis.HasKey(Symbol) then
  begin
    FThis.SetValue(Symbol,Value,False);
    Result := True;
    Exit;
  end;

  //親を探す
  if Assigned(FParent) then
    Result := FParent.SetValueImpl(Self,Symbol,Value)
  else
    Result := False;
end;

procedure TJLocalSymbolTable.LocalCopy(Source: TJLocalSymbolTable);
//sourceからコピーする
var
  i: Integer;
  sl: TStringList;
  v: TJValue;
begin
  if not Assigned(Source) then
    Exit;

  //先の値を優先する（上書きしない)
  sl := Source.FLocal.KeyList;
  for i := 0 to sl.Count - 1 do
  begin
    if not FLocal.HasKey(sl[i]) then
    begin
      if Source.FLocal.GetValue(sl[i],v) then
        FLocal.SetValue(sl[i],v);
    end;
  end;
end;

procedure TJLocalSymbolTable.AddTemporaryObject(AObject: TJObject;
  IncRef: Boolean = True);
//temp objectを加えるだけ
begin
  FTempObjects.Add(AObject,IncRef);
end;

function TJLocalSymbolTable.GetNodeTable: TJFunctionSymbolTable;
//functionテーブルを得る
var
  table: TJLocalSymbolTable;
begin
  Result := nil;
  table := Self;
  while Assigned(table) do
  begin
    if table is TJFunctionSymbolTable then
    begin
      Result := table as TJFunctionSymbolTable;
      Break;
    end
    else //親へ移動
      table := table.FParent;
  end;
end;

procedure TJLocalSymbolTable.SetParent(Value: TJLocalSymbolTable);
//親を再設定
begin
  FParent := Value;
end;

procedure TJLocalSymbolTable.SetThis(const Value: TJObject);
begin
  if Assigned(FThis) then
  begin
    //終了通知を消す
    FThis.RemoveFreeNotification(Self);
    RemoveFreeNotification(FThis);
  end;
  //通知を付ける
  if Assigned(Value) then
    Value.FreeNotification(Self);
  //入れ替え
  FThis := Value;
end;

procedure TJLocalSymbolTable.Notification(AObject: TJNotify);
//終了通知
begin
  inherited;
  if AObject = FThis then
    FThis := nil
  else if AObject = FParent then
    FParent := nil;
end;

procedure TJLocalSymbolTable.RegistStaticValue(Symbol: String;
  Value: TJValue);
//関数のnodeにstatic登録する
var
  table: TJFunctionSymbolTable;
begin
  table := GetNodeTable;
  //値の上書きを防ぐため登録は１回だけ
  if not table.FLocal.HasKey(Symbol) then
    table.RegistValue(Symbol,Value);
end;

procedure TJLocalSymbolTable.ClearTemporaryObject;
begin
  FTempObjects.Clear;
end;

{ TJFunctionSymbolTable }

function TJFunctionSymbolTable.GetValueImpl(Caller: TJLocalSymbolTable;
  Symbol: String; var Value: TJValue): Boolean;
//値を得る
begin
  Result := False;
  //呼び出しが下位だったらなにもしない
  if FTables.IndexOf(Caller) > -1 then
  //下位ローカルテーブルがあれば
  else if FTables.Count > 0 then
  begin
    //最初のテーブルを検索
    Result :=
        (FTables[0] as TJLocalSymbolTable).GetValueImpl(Self,Symbol,Value);
  end;
  //無ければ次へ
  if not Result then
    Result := inherited GetValueImpl(Self,Symbol,Value);
end;

function TJFunctionSymbolTable.SetValueImpl(Caller: TJLocalSymbolTable;
  Symbol: String; var Value: TJValue): Boolean;
//値をセット
begin
  Result := False;
  //呼び出しが下位だったらなにもしない
  if FTables.IndexOf(Caller) > -1 then
  //下位ローカルテーブルがあれば
  else if FTables.Count > 0 then
  begin
    //最初のテーブルを検索
    Result :=
        (FTables[0] as TJLocalSymbolTable).SetValueImpl(Self,Symbol,Value);
  end;
  //無ければ次へ
  if not Result then
    Result := inherited SetValueImpl(Self,Symbol,Value);
end;


{ TJRootSymbolTable }

procedure TJRootSymbolTable.Clear;
begin
  inherited;
  FFunctions.Clear;
  FGlobals.Clear;
end;

constructor TJRootSymbolTable.Create(AParent: TJLocalSymbolTable);
begin
  inherited;
  FFunctions := TObjectHashTable.Create(HASH_50);
  FGlobals := TStringList.Create;
  FGlobals.Sorted := True;
  FGlobals.Duplicates := dupIgnore;
end;

destructor TJRootSymbolTable.Destroy;
begin
  inherited;
  FreeAndNil(FFunctions);
  FreeAndNil(FGlobals);
end;

function TJRootSymbolTable.GetFunctionTable(
  AParent,AFunc: PJStatement): TJFunctionSymbolTable;
//function tableを作成する
var
  patable: TJFunctionSymbolTable;
  pastr,fustr: String;
begin
  //自分を探す
  fustr := IntToStr(Integer(AFunc));
  if FFunctions.HasKey(fustr) then
    Result := FFunctions[fustr] as TJFunctionSymbolTable
  else begin //作成
    //親テーブルを捜す
    pastr := IntToStr(Integer(AParent));
    if FFunctions.HasKey(pastr) then
      patable := FFunctions[pastr] as TJFunctionSymbolTable
    else
      patable := nil;
    //テーブルを作成
    Result := TJFunctionSymbolTable.Create(patable);
    FFunctions[fustr] := Result;
  end;
end;

function TJRootSymbolTable.GetFunctionTable(AParent: TJFunctionSymbolTable;
  AFunc: PJStatement): TJFunctionSymbolTable;
//テーブルを作成
//function tableを作成する
var
  fustr: String;
begin
  //自分を探す
  fustr := IntToStr(Integer(AFunc));
  if FFunctions.HasKey(fustr) then
    Result := FFunctions[fustr] as TJFunctionSymbolTable
  else begin
    //テーブルを作成
    Result := TJFunctionSymbolTable.Create(AParent);
    FFunctions[fustr] := Result;
  end;
end;

function TJRootSymbolTable.FindGlobalTable(AName: String): TJGlobalSymbolTable;
//globalテーブルを探す
var
  index: Integer;
begin
  if FGlobals.Find(AName,index) then
    Result := FGlobals.Objects[index] as TJGlobalSymbolTable
  else
    raise EJThrow.Create(E_NAME,'namespace not found: ' + AName);
end;

function TJRootSymbolTable.MakeGlobalTable(AName: String;
  AFunc: PJStatement): TJGlobalSymbolTable;
//GlobalTableを作成する
begin
  //作成
  Result := TJGlobalSymbolTable.Create(Self);
  FGlobals.AddObject(AName,Result);
  FFunctions[IntToStr(Integer(AFunc))] := Result;
end;


{ EJSyntaxError }

constructor EJSyntaxError.Create(ALineNo: Integer; AMsg: String;
  AValue: PJValue = nil);
begin
  inherited Create(E_SYNTAX,AMsg,AValue);
  FLineNo := ALineNo;
end;

{ TJPrototypeObject }

constructor TJPrototypeObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Prototype');
end;

destructor TJPrototypeObject.Destroy;
begin
  //減らす
  if Assigned(FPrototype) then
    FPrototype.DecRef;

  inherited;
end;

procedure TJPrototypeObject.GetPropertyList(List: TStringList);
begin
  //全てのメンバー
  GetKeyList(List,[],[]);
end;

function TJPrototypeObject.GetPrototype: TJObject;
begin
  if Assigned(FPrototype) then
    Result := FPrototype
  else //無ければ自分
    Result := Self;
end;

procedure TJPrototypeObject.Notification(AObject: TJNotify);
//objectの終了通知
begin
  inherited;
  if AObject = FPrototype then
    FPrototype := nil;
end;

procedure TJPrototypeObject.RegistMethods;
begin
  //何もしない
end;

procedure TJPrototypeObject.SetPrototype(const Value: TJObject);
begin
  //増やす
  if Assigned(Value) then
    Value.IncRef;

  if Assigned(FPrototype) then
  begin
    //通知を削除
    FPrototype.RemoveFreeNotification(Self);
    RemoveFreeNotification(FPrototype);
    //減らす
    FPrototype.DecRef;
  end;

  //入れ替え
  FPrototype := Value;
  //通知をセット
  if Assigned(FPrototype) then
    FPrototype.FreeNotification(Self);
end;


{ TJFunctionImpl }

constructor TJFunctionImpl.Create;
begin
  inherited Create;
  FNotify := TJNotify.Create;
  FNotify.OnNotification := NotifyOnNotifycation;
end;

destructor TJFunctionImpl.Destroy;
begin
  FMethodOwner := nil;
  //存在すれば削除
  FreeAndNil(FLocalTable);
  FreeAndNil(FNotify);
  inherited;
end;

function TJFunctionImpl.GetFunctionTable: TJFunctionSymbolTable;
begin
  Result := FFunc.FunctionTable;
end;

function TJFunctionImpl.GetFlag: TJFunctionCallFlag;
begin
  Result := FFunc.Flag;
end;

function TJFunctionImpl.GetFuncType: TJFuncType;
begin
  Result := FFunc.FuncType;
end;

function TJFunctionImpl.GetParameter: PJStatement;
begin
  Result := FFunc.Parameter;
end;

function TJFunctionImpl.GetSymbol: String;
begin
  Result := FFunc.Symbol;
end;

function TJFunctionImpl.GetvActiveX: PJActiveXMethod;
begin
  Result := @FFunc.vActiveX;
end;

function TJFunctionImpl.GetvDynaCall: PDynaDeclare;
begin
  Result := @FFunc.vDynaCall;
end;

function TJFunctionImpl.GetvMethod: TJMethod;
begin
  Result := FFunc.vMethod;
end;

function TJFunctionImpl.GetvStatement: PJStatement;
begin
  Result := FFunc.vStatement;
end;

procedure TJFunctionImpl.SetFunctionTable(const Value: TJFunctionSymbolTable);
begin
  FFunc.FunctionTable := Value;
end;

procedure TJFunctionImpl.SetFlag(const Value: TJFunctionCallFlag);
begin
  FFunc.Flag := Value;
end;

procedure TJFunctionImpl.SetFuncType(const Value: TJFuncType);
begin
  FFunc.FuncType := Value;
end;

procedure TJFunctionImpl.SetParameter(const Value: PJStatement);
begin
  FFunc.Parameter := Value;
end;

procedure TJFunctionImpl.SetSymbol(const Value: String);
begin
  FFunc.Symbol := Value;
end;

procedure TJFunctionImpl.SetvMethod(const Value: TJMethod);
begin
  FFunc.vMethod := Value;
end;

procedure TJFunctionImpl.SetvStatement(const Value: PJStatement);
begin
  FFunc.vStatement := Value;
end;

function TJFunctionImpl.GetLocalTable: TJLocalSymbolTable;
//local tableを返す
begin
  Result := FLocalTable
end;

procedure TJFunctionImpl.SetLocalTable(const Value: TJLocalSymbolTable);
begin
  if Assigned(FLocalTable) then
    FreeAndNil(FLocalTable);

  FLocalTable := Value;
end;

procedure TJFunctionImpl.Assign(Source: IJFunction);
//コピーする
begin
  FFunc := Source.GetFunc;
  if FMethodOwner <> Source.MethodOwner then
    SetMethodOwner(Source.MethodOwner);

  //localをコピー
  if Assigned(Source.LocalTable) then
  begin
    if not Assigned(FLocalTable) then
      FLocalTable := TJLocalSymbolTable.Create(nil);
    //コピー
    FLocalTable.LocalCopy(Source.LocalTable);
  end;
end;

function TJFunctionImpl.GetFunc: __TJFunction;
begin
  Result := FFunc;
end;

function TJFunctionImpl.GetMethodOwner: TJObject;
//関数の所有クラス
begin
  Result := FMethodOwner;
end;

procedure TJFunctionImpl.SetMethodOwner(const Value: TJObject);
begin
  //通知を消す
  if Assigned(FMethodOwner) then
  begin
    FMethodOwner.RemoveFreeNotification(FNotify);
    FNotify.RemoveFreeNotification(FMethodOwner);
  end;
  //入れ替え
  FMethodOwner := Value;
  //終了通知をセット
  if Assigned(FMethodOwner) then
    FMethodOwner.FreeNotification(FNotify);
end;

procedure TJFunctionImpl.NotifyOnNotifycation(Sender: TObject);
//owner objectの破棄
begin
  FMethodOwner := nil;
end;

{ TJNotify }

destructor TJNotify.Destroy;
//notifyをすべて起動
var
  i: Integer;
begin
  if Assigned(FFreeNotifies) then
  begin
    for i := FFreeNotifies.Count - 1 downto 0 do
    begin
      TJNotify(FFreeNotifies[i]).Notification(Self);
      if not Assigned(FFreeNotifies) then
        Break;
    end;

    FreeAndNil(FFreeNotifies);
  end;

  inherited;
end;

procedure TJNotify.FreeNotification(AObject: TJNotify);
//notifyを登録する
begin
  //作成
  if not Assigned(FFreeNotifies) then
    FFreeNotifies := TBinList.Create;
  //すでに登録してなければ
  if FFreeNotifies.IndexOf(AObject) < 0 then
  begin
    //お互いに登録
    FFreeNotifies.Add(AObject);
    AObject.FreeNotification(Self);
  end;
end;

procedure TJNotify.Notification(AObject: TJNotify);
begin
  if Assigned(AObject) then
  begin
    //AObjectを自分から消す
    RemoveFreeNotification(AObject);
    //イベントがあればイベント
    if Assigned(FOnNotification) then
      FOnNotification(AObject); //AObjectを送る
  end;
end;

procedure TJNotify.RemoveFreeNotification(AObject: TJNotify);
//登録を解除
begin
  if Assigned(FFreeNotifies) then
  begin
    FFreeNotifies.Remove(AObject);
    if FFreeNotifies.Count = 0 then
      FreeAndNil(FFreeNotifies);
  end;
end;

end.
