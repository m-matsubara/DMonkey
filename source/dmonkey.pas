unit DMonkey;

(*
  DMS(DMonkey Script) by Project DMonkey
  License: BSD
           このライブラリは無保証です。
           使用、改変、配布に一切の制限はありません。
           作者に通知やライセンス表示も必要ありません。
  History:
  2010/04/29 ver.0.3.9.1 (Unicode対応) m.matsubara
          Unicode対応
          Bytes, Encoding, FileReader, FileWriter型の追加
          String型でShiftJISやEUC, JISコードの相互変換メソッドを廃止
          File型はバイナリのみを扱う仕様に変更
            readln(), writeln()の廃止
            read() の戻り値をStringからBytesに変更
            write() の引数をStringからBytesに変更
          ※このUnicode対応版ではString型が特定の文字コードであることに依存するようなコードを書くことは推奨されません。

  2005/08/07 ver.0.3.9.1
          COMオブジェクトにオブジェクトの値が渡らないことがあるのを修正
          メッセージボックスやフォームを表示するメソッドの引数を拡張
          TJValueList.SetCountで要素数を減らしたとき1要素余計に削除してたのを修正
          TJDynaCall.SendMessageImplで引数がnullのときも参照を渡してたのを修正
          DynaCall.register()で参照指定(1,2,4,8)の引数にnullを渡したときは0(NULL)を渡すよう変更

  2005/07/29 ver.0.3.9
          Global.nameOf()でオブジェクト名を取得(コンストラクタと同じとは限らない)
          DynaCall.sendMessage() .postMessage()の引数の型を明示指定する仕様に変更
          StringBuffer.substr()の修正
          String.substring() StringBuffer.substring()で引数省略時は最後まで取り出すように変更
          ecma_misc.pas MB関数群を修正

  2005/07/24 ver.0.3.8.1
          TJEngine.Run(), TJEngine.CallFunction()の修正

  2005/07/23 ver.0.3.8
          StringBuffer.indexOf() .lastIndexOf()でAnsiPosを使わないよう変更
          TJStringBufferObjectの添字での代入を文字列から文字に変更
          String.multiply()
          String.charAt() .charCodeAt()でインデックスの範囲チェックするよう修正
          Math.max() .min()で3つ以上引数を取れるよう変更
          StringBuffer.charCodeAt()でインデックスの範囲チェックするよう修正
          VCLListBox.clearSelection() .deleteSelected() .selectAll()
          Global.isFinite()
          TJActiveXObject.GetValueで2つ以上の引数を渡せるよう修正(Excel.cells対策)
          DynaCallの参照渡しパラメータ(1,2,4,8)にNumberオブジェクトを渡すと値が反映
          Number.asCharで数値(文字コード)を文字で取得/設定
          例外、中止時にもイベントループに入ってしまう場合があるのを修正

  2005/07/08 ver.0.3.7
          DynaCall.copyMemory() .fillMemory()
          StringBuffer.fill()
          DynaCall.sendMessage() .postMessage()でStructもポインタを渡すよう修正
          String.slice()の修正

  2005/07/05 ver.0.3.6.1
          Object.getKeys() .getMethods() .getProperties()の修正

  2005/06/26 ver.0.3.6
          TJHTTPObject.DoResponse()でプロトコルエラーの場合でもヘッダを取得するよう修正
          Strings.caseSensitive .duplicates
          regexpr.pasをver0.952に
          StringBuffer.indexOf() .lastIndexOf() .slice() .substr()
          StringBuffer.substring()を修正
          VCLListView.ItemIndex
          Dialog.filters
          ecma_type.Get/SetDefaultPropertyでChar型を扱えるよう修正
          @VERSION7適用時にfor..inでアクセス違反が起こることがあったのを修正
          File,Directoryの一部メソッドが引数をとれるように
           (省略時は今まで通りfilename,dirnameプロパティの文字列が対象)
          Keyboard.isDown()に仮想キーコードを渡せるように変更
          TJRegExpObject.ToString()
          正規表現リテラルがecma_type.AsString(),TypeOf()で返す値を変更
          String.charCodeAt() .fromCharCode()を2バイト文字も扱えるように
          Struct.clear() .define()
          Number.toString()で8進数にも変換できるように

  2003/05/20 ver.0.3.5
          WScript.Argumentsに前回実行時の引数が渡ってたのを修正
          代入演算子^=が使えなかったのを修正
          Global.platformでOS判別 'win32s'|'windows'|'nt'
          TJEngine.EvalStatementの最後でtemporary objectを開放するように修正

  2003/04/11 ver.0.3.4
          String.lastIndexOf()の修正
          TJStructの修正
          VCLMemo.textプロパティ

  2003/04/09 ver.0.3.3
          TJEngine.CallArrayExpr()の修正

  2003/04/08 ver.0.3.2
          関数のシリアライズがおかしかったのを修正

  2003/04/07 ver.0.3.1
          単項演算子+を修正
          値型文字列の参照文字列への変換を修正
          Date.formatで書式指定('ggee yyyy/mm/dd(aaaa) ampm hh:nn:ss')
          配列の除算がsyntaxエラーになっていたのを修正
          /*...**/だとコメントが終了しないのを修正。
          DynaCallで登録した関数の's'パラメータにnullを指定したときはNULL(0)を渡すようにした。
          DynaCallオブジェクトにsendMessage()メソッドを追加。
          DynaCallオブジェクトにpostMessage()メソッドを追加。
          Structオブジェクトを追加。
          StringBufferオブジェクトの作成時引数を拡張。
          Structオブジェクトの型に'i','w'を追加。
          Numberオブジェクトの比較がおかしかったのを修正。
          NaNの扱いを少し変更。
          Global.msgBox()の引数を拡張。

  2003/02/23 ver.0.3.0
          サンプルを修正
          TJObjectのデフォルトプロパティでNameとTagを無視していたのを修正
          関数式
          HTTPSがまともに動いてなかったのを修正
          LibraryPathにカレントディレクトリを追加
          object作成式にQuoteStringとNumberを使えるよう修正 { "a" : 0 }
          Stringオブジェクトを作り直し
          StringBufferオブジェクト 内部はString
          Number.toChar()で数字の文字コードを文字に変更
          ()と[]の扱いを同じにする
          関数への代入...引数の最後に代入値を追加します
          newでのObjecy作成で関数名を指定するとコンストラクタ
          OnErrorイベント
          終了時のエラーを修正
          $を変数に使えるよう修正
          TJCookieObjectを修正
          TJArrayObject.GetValueを変更
          prototype
          Date.getTime()をミリ秒単位に修正
          delete式を修正
          TJStringsObject.ToStringを修正
          Directory.clear() .findfiles() .files .directories
          Global.format() .formatFloat()
          CheckListBox.index
          ecma_expr.pasのCalcValue等をecma_type.pasへ移動
          function.call() .apply() .callee()
          Array.assign()
          File.path
          Directory.path
          undefinedを返すメソッドをthisオブジェクトを返すように修正
          TJObject.ToInteger .ToDoubleを削除して .ToNumberに統一
          CalcValue2でオブジェクトの演算を修正
          変数宣言でvarがあるとローカル、無い場合はグローバル(TDMonkey.DeclareLocalVar)
          ActiveXのイベント
          VCLオブジェクト
          OnDoEventsイベント
          GarbageCollectプロパティを削除したのでコンポーネントを再登録してください
          集合型と列挙型プロパティは文字列に変換する
          TJObject.Createの引数を変更
          RegExpの内部を少し変更
          eval()をまともに動くよう修正
          TJNotifyを使うとObjectの終了通知を受けることができます
          static・global宣言で静的変数(文法はvarと同じ)
          try catch finallyを修正
          ソケットオブジェクトにイベント
          TJIniObject.update() writeを使った後iniを更新するためにupdateをしてください（気が付きませんでした…）
          TJBaseArrayObjectの仮想メソッドをTJObjectへ移動
          String.replace()でサブマッチ$nの置換、置換文字列に関数を指定
          正規表現のオブジェクトの省略メンバ  $& $' $* $+ $_ $`
          Object.getKeys() .getProperties() .getMethods()はArrayを返すように変更

  2002/12/29 ver.0.2.1
          caseを修正
          条件コンパイル文 @set @xxx = [bool|int]
          for..in文でのArrayをデフォルトでインデックスに設定 「@set @VERSION7 = true」 で要素
          文に行番号を含める
          正規表現リテラルがメモリリークする事があったのを修正
          @set @SHORT_CIRCUIT = false で条件式の完全評価
          Global.encodeURI,encodeURIComponent,decodeURI,decodeURIComponent

  2002/12/26 ver.0.2.0.3
          caseを連続で並べられなかったのを修正

  2002/12/26 ver.0.2.0.2
          ラベルに符号付数字が使えなかったのを修正

  2002/12/25 ver.0.2.0.1
          ActiveXのPropertyGetを修正

  2002/12/23 ver.0.2.0
          shobohn氏のコードをマージ
          拡張子の定義(ecma_type.pas)
          拡張Objectを選択インポートするための$DEFINE
          Complie時にLibPathを追加する
          eval修正
          参照カウントのコードを除去
          eの前が実数のとき弾いてたのを修正。(ecma_lex.pas)
          メッセージボックスのオーナーの有無を指定する$DEFINEを追加。(ecma_misc.pas)
          String.crypt([salt]) Unix等のDES crypt(3)。Perl互換です。
          OnStdinイベントとGlobal.read() Global.readln()
          正規表現リテラル /patern/ig
          switch文を実装
          Global.scriptEngineVersion()
          constructorで例外を起こすobjectのアクセス違反を修正
          コンパイル済みバイナリ(拡張子 .dmc)
          for(var i=0; ... がエラーにならないよう修正
          新しい参照カウント
          FTPオブジェクト
          RegExp.replaceを修正
          Global.isConsole()
          配列を()でアクセス  a = [1]; println(a(0));
          Global.argsを削除
          WScriptオブジェクト（不完全）
          ActiveXObjectのプロパティ呼び出しを修正
          RegExp.multiline と mオプション
          Enumeratorオブジェクト
          for..in文でコレクションとArrayの場合は要素を返すように修正
          抽象ArrayオブジェクトのTJBaseArrayObjectを定義(count,lengthを持ちfor..inを使うObjectは継承することを推奨)
          TJStringsをTJBaseArrayObject継承に変更
          TJHtmlParserObjectをTJBaseArrayObject継承に変更
          String.trim() trimLeft() trimRight() 制御・空白文字を削除
          TJStringObjectをTJBaseArrayObject継承に変更
          関数内でvarがエラーになっていたのを修正
          配列要素が1つの時、配列数になっていたのを修正 a = [5]

  2002/05/25 ver.0.1.7
          scriptEngine()などを定義

  2002/05/20 ver.0.1.6
          HtmlParser
          RegExp.test()の修正
          その他

  2002/05/15 ver.0.1.5
          Dateの修正

  2002/05/11 ver.0.1.4
          スクリプトの全角空白を無視するように修正
          RegIni
          Dateの修正

  2002/04/23 Ver.0.1.3
          Objectの２次式を修正

  2002/04/14 Ver.0.1.2
          TJObjectFactoryの仕様を変更（重要）これによりTJObjectはコンストラクタ引数のTJObjectFactoryに自動的に所有されます。NewObjectメソッドは使用しないでください。
          プロパティにFactoryを追加
          参照カウントの修正（USE_GC条件が必要）
          CheckListBox
  2002/03/21 Ver.0.1.1
          DynaCallの修正
  2002/03/20 Ver.0.1.0
          イベントの登録を変更
          DynaCallオブジェクト
  2002/03/10 Ver.0.0.15
          Dateの月を0～11に変更
          Arrayオブジェクトの初期化
          OnStepイベントを追加(スクリプトの中断などに使用)
          正規表現ライブラリの変更
          String.toUTF8()を追加
          String.fromUTF8toSJIS()を追加
  2002/03/06 Ver.0.0.14
          Objectメンバの参照カウントを修正
          配列式の修正
          Arrayオブジェクトの修正
  2002/02/07 Ver.0.0.13
          メンバ式を修正
  2002/02/06 Ver.0.0.12
          IDispatchの呼び出しを修正
  2002/02/02 Ver.0.0.11
          Clipboardオブジェクト
  2002/02/01 Ver.0.0.10
          ActiveXメソッドとプロパティ呼び出しを修正
  2002/01/28 Ver.0.0.9
            KeyboardとMouseオブジェクト
  2002/01/27 Ver.0.0.8
            バグ修正
  2001/11/16 Ver.0.0.7
            エラー出力
          published propertyの文字ケースを無視
  2001/05/09 Ver.0.0.6
            var
  2001/05/06 Ver.0.0.5
            import
  2001/05/04 Ver.0.0.4
            クラス定義
  2001/05/04 Ver.0.0.3
            ActiveXObject
  2001/05/02 Ver.0.0.2
          breakとcontinueを修正
  2001/04/30 Ver.0.0.1
          初版
*)


{$IFDEF CONSOLE}
  {$DEFINE NO_VCL}
  {$DEFINE NO_GUI}
{$ENDIF}

{$IFDEF UNICODE}
  // こののUnicode対応DMonkeyはソケット関連の型をUnicode対応していません。
  {$DEFINE NO_SOCKET}
{$ENDIF}


interface

uses
  Windows, SysUtils, Classes,
  ecma_lex,ecma_parser,ecma_type,ecma_engine,
{$IFNDEF NO_EXTENSION}
  ecma_extobject,
{$ENDIF}
{$IFNDEF NO_SOCKET}
  ecma_sockobject,
{$ENDIF}
{$IFNDEF NO_ACTIVEX}
  ecma_activex,
{$ENDIF}
{$IFNDEF NO_DYNACALL}
  ecma_dynacall,
{$ENDIF}
{$IFNDEF NO_GUI}
  ecma_guiobject,
{$ENDIF}
{$IFNDEF NO_VCL}
  ecma_vcl,
{$ENDIF}
  ecma_object,ecma_misc;

type
  TDMonkey = class(TJBaseDMonkey)
  private
    FEngine: TJEngine;

    FErrorText: String;
    FTookTimeToCompile: Cardinal;
    FTookTimeToRun: Cardinal;
    FTookTimeToCallFunction: Cardinal;
    FCompiledBinary: Boolean;
    //イベント
    FOnStdout: TStringEvent;
    FOnDebugout: TStringEvent;
    FOnNewObject: TNewObjectEvent;
    FOnStderr: TStringEvent;
    FOnRun: TNotifyEvent;
    FOnDone: TNotifyEvent;
    FOnStep: TStepEvent;
    FOnStdin: TReadStringEvent;
    FOnError: TErrorEvent;
    FOnDoEvents: TStepEvent;
    //
    procedure EngineOnDebug(Sender: TObject; S: String);
    procedure EngineOnStdout(Sender: TObject; S: String);
    procedure EngineOnStderr(Sender: TObject; S: String);
    procedure EngineOnNewObject(Sender: TObject; JObject: TJObject);
    procedure EngineOnRun(Sender: TObject);
    procedure EngineOnDone(Sender: TObject);
    procedure EngineOnStep(Sender: TObject; var AbortScript: Boolean);
    procedure EngineOnStdin(Sender: TObject; var S: String; var Success: Boolean;
      Count: Integer; Line: Boolean);
    procedure EngineOnError(Sender: TObject; LineNo: Integer; Msg: String);
    procedure EngineOnDoEvents(Sender: TObject; var AbortScript: Boolean);

    function GetLibraryPath: TStrings;
    procedure SetLibraryPath(const Value: TStrings);
    function GetObjectCount: Integer;
    procedure SetOnDone(const Value: TNotifyEvent);
    procedure SetOnNewObject(const Value: TNewObjectEvent);
    procedure SetOnRun(const Value: TNotifyEvent);
    procedure SetOnStderr(const Value: TStringEvent);
    procedure SetOnStep(const Value: TStepEvent);
    procedure SetOnStdout(const Value: TStringEvent);
    procedure SetOnStdin(const Value: TReadStringEvent);
    function GetLineNumber: Integer;
    procedure SetOnError(const Value: TErrorEvent);
    procedure SetOnDebugout(const Value: TStringEvent);
    function GetFilename: String;
    procedure SetOnDoEvents(const Value: TStepEvent);
    function GetFactory: TJObjectFactory;
    function GetRegistVar: TJRegistVarType;
    procedure SetRegistVar(const Value: TJRegistVarType);

  protected
    procedure RegistDMSObjects; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Compile(SourceCode: String): Boolean;
    function CompileFile(AFilename: String): Boolean;
    function Run(Args: array of const): Integer; overload;
    function Run(Args: TJValueList): Integer; overload;
    function Run: Integer; overload;
    function CallFunction(Symbol: String; Param: array of const; var RetValue: TJValue): Boolean; overload;
    function CallFunction(Symbol: String; Param: TJValueList; var RetValue: TJValue): Boolean; overload;
    procedure Clear;
    procedure Abort;
    procedure ImportObject(ObjectName: String; ObjectClass: TJObjectClass);
    function IsRunning: Boolean;
    class function ScriptBuild: Integer;
    class function ScriptEngine: String;
    class function ScriptVersion: String;

    property ObjectCount: Integer read GetObjectCount;
    property Factory: TJObjectFactory read GetFactory;
    property TookTimeToCompile: Cardinal read FTookTimeToCompile write FTookTimeToCompile;
    property TookTimeToRun: Cardinal read FTookTimeToRun write FTookTimeToRun;
    property TookTimeToCallFunction: Cardinal read FTookTimeToCallFunction write FTookTimeToCallFunction;
    property ScriptFilename: String read GetFilename;
    property LineNumber: Integer read GetLineNumber;
    //var宣言なし変数登録の動作
    property RegistVar: TJRegistVarType read GetRegistVar write SetRegistVar;
  published
    property LibraryPath: TStrings read GetLibraryPath write SetLibraryPath;
    property CompiledBinary: Boolean read FCompiledBinary write FCompiledBinary;
    //イベント
    property OnStdout: TStringEvent read FOnStdout write SetOnStdout;
    property OnStderr: TStringEvent read FOnStderr write SetOnStderr;
    property OnDebugout: TStringEvent read FOnDebugout write SetOnDebugout;
    property OnNewObject: TNewObjectEvent read FOnNewObject write SetOnNewObject;
    property OnRun: TNotifyEvent read FOnRun write SetOnRun;
    property OnDone: TNotifyEvent read FOnDone write SetOnDone;
    property OnStep: TStepEvent read FOnStep write SetOnStep;
    property OnStdin: TReadStringEvent read FOnStdin write SetOnStdin;
    property OnError: TErrorEvent read FOnError write SetOnError;
    property OnDoEvents: TStepEvent read FOnDoEvents write SetOnDoEvents;
  end;

  TDMS = class(TDMonkey);


procedure ShowDMonkeyException(DMonkey: TDMonkey);


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDMS]);
end;


procedure ShowDMonkeyException(DMonkey: TDMonkey);
//エラーを表示
var
  caption,text: String;
begin
  caption := GetApplicationTitle;
  if DMonkey.ScriptFilename <> '' then
    caption := caption + ' - ' + ExtractFilename(DMonkey.ScriptFilename);

  text := DMonkey.FErrorText;
  MsgBox(PChar(text), PChar(caption), MB_OK or MB_ICONHAND);
end;


{ TDMonkey }

procedure TDMonkey.Abort;
//中止する
begin
  FEngine.Abort;
end;

function TDMonkey.CallFunction(Symbol: String; Param: TJValueList; var RetValue: TJValue): Boolean;
//関数呼び出し
var
  tmp: Cardinal;
begin
  //時間計測
  tmp := GetTickCount;
  try
    Result := FEngine.CallFunction(Symbol,Param,RetValue);
  finally
    FTookTimeToCallFunction := GetTickCount - tmp;
  end;
end;

function TDMonkey.CallFunction(Symbol: String;
  Param: array of const; var RetValue: TJValue): Boolean;
//関数呼び出し
var
  list: TJValueList;
  i: Integer;
  v: TJValue;
begin
  list := TJValueList.Create;
  try
    //変換
    for i := 0 to High(Param) do
    begin
      v := VarRecToValue(Param[i]);
      //参照カウントを増やす
      if IsObject(@v) then
        v.vObject.IncRef;

      list.Add(v);
    end;

    Result := CallFunction(Symbol,list,RetValue);
  finally
    list.Free;
  end;
end;

procedure TDMonkey.Clear;
//データをクリア
begin
  FEngine.Clear;
end;

function TDMonkey.Compile(SourceCode: String): Boolean;
//解析木を作る
var
  tmp: Cardinal;
begin
  //コンパイル時間
  tmp := GetTickCount;
  try
    Result := FEngine.Compile(SourceCode);
  finally
    //コンパイル時間
    FTookTimeToCompile := GetTickCount - tmp;
  end;
end;

function TDMonkey.CompileFile(AFilename: String): Boolean;
var
  tmp: Cardinal;
begin
  tmp := GetTickCount;
  try
    Result := FEngine.CompileFile(AFilename,FCompiledBinary);
  finally
    //コンパイル時間
    FTookTimeToCompile := GetTickCount - tmp;
  end;
end;

constructor TDMonkey.Create(AOwner: TComponent);
//作成
begin
  inherited;
  FEngine := TJEngine.Create(Self);
  //拡張objectをインポートする
  RegistDMSObjects;
end;

destructor TDMonkey.Destroy;
//破棄
begin
  Clear;
  FreeAndNil(FEngine);
  inherited;
end;

procedure TDMonkey.EngineOnDone(Sender: TObject);
begin
 if Assigned(FOnDone) then
   FOnDone(Self);
end;

procedure TDMonkey.EngineOnNewObject(Sender: TObject; JObject: TJObject);
//object 作成イベント
begin
  if Assigned(FOnNewObject) then
    FOnNewObject(Self,JObject);
end;

procedure TDMonkey.EngineOnRun(Sender: TObject);
begin
  if Assigned(FOnRun) then
    FOnRun(Self);
end;

procedure TDMonkey.EngineOnStderr(Sender: TObject; S: String);
//標準エラー
begin
  FErrorText := S;
  if Assigned(FOnStderr) then
    FOnStderr(Self,S);
end;

procedure TDMonkey.EngineOnStdout(Sender: TObject; S: String);
//標準出力
begin
  if Assigned(FOnStdout) then
    FOnStdout(Self,S);
end;

procedure TDMonkey.EngineOnStep(Sender: TObject; var AbortScript: Boolean);
begin
  if Assigned(FOnStep) then
    FOnStep(Self,AbortScript);
end;

function TDMonkey.GetLibraryPath: TStrings;
begin
  Result := FEngine.Parser.LibPath;
end;

function TDMonkey.GetObjectCount: Integer;
begin
  Result := FEngine.ObjectCount;
end;

procedure TDMonkey.ImportObject(ObjectName: String;
  ObjectClass: TJObjectClass);
//組込みオブジェクトをインポート
begin
  FEngine.ImportObject(ObjectName,ObjectClass);
end;

function TDMonkey.IsRunning: Boolean;
begin
  Result := FEngine.IsRunning;
end;

procedure TDMonkey.EngineOnDebug(Sender: TObject; S: String);
//デバッグ
begin
  if Assigned(FOnDebugout) then
    FOnDebugout(Self,S);
end;

function TDMonkey.Run(Args: array of const): Integer;
//scriptを実行
var
  i: Integer;
  param: TJValueList;
begin
  param := TJValueList.Create;
  try
    for i := 0 to High(Args) do
      param.Add(VarRecToValue(Args[i]));

    Result := Run(param);
  finally
    param.Free;
  end;
end;

function TDMonkey.Run(Args: TJValueList): Integer;
//実行
var
  tmp: Cardinal;
begin
  //実行時間
  tmp := GetTickCount;
  try
    Result := FEngine.Run(nil,Args);
  finally
    FTookTimeToRun := GetTickCount - tmp;
  end;
end;

function TDMonkey.Run: Integer;
//実行引数無し
begin
  Result := Run([]);
end;

procedure TDMonkey.SetLibraryPath(const Value: TStrings);
begin
  FEngine.Parser.LibPath.Assign(Value);
end;

procedure TDMonkey.SetOnDone(const Value: TNotifyEvent);
begin
  FOnDone := Value;
  if Assigned(Value) then
    FEngine.OnDone := EngineOnDone
  else
    FEngine.OnDone := nil;
end;

procedure TDMonkey.SetOnNewObject(const Value: TNewObjectEvent);
begin
  FOnNewObject := Value;
  if Assigned(Value) then
    FEngine.OnNewObject := EngineOnNewObject
  else
    FEngine.OnNewObject := nil;
end;

procedure TDMonkey.SetOnRun(const Value: TNotifyEvent);
begin
  FOnRun := Value;
  if Assigned(Value) then
    FEngine.OnRun := EngineOnRun
  else
    FEngine.OnRun := nil;
end;

procedure TDMonkey.SetOnStderr(const Value: TStringEvent);
begin
  FOnStderr := Value;
  if Assigned(Value) then
    FEngine.OnStdErr := EngineOnStdErr
  else
    FEngine.OnStdErr := nil;
end;

procedure TDMonkey.SetOnStep(const Value: TStepEvent);
begin
  FOnStep := Value;
  if Assigned(Value) then
    FEngine.OnStep := EngineOnStep
  else
    FEngine.OnStep := nil;
end;

procedure TDMonkey.SetOnStdout(const Value: TStringEvent);
begin
  FOnStdout := Value;
  if Assigned(Value) then
    FEngine.OnStdOut := EngineOnStdOut
  else
    FEngine.OnStdOut := nil;
end;

class function TDMonkey.ScriptBuild: Integer;
begin
  Result := DMS_BUILD;
end;

class function TDMonkey.ScriptEngine: String;
begin
  Result := DMS_ENGINE;
end;

class function TDMonkey.ScriptVersion: String;
begin
  Result := DMS_VERSION;
end;

procedure TDMonkey.EngineOnStdin(Sender: TObject; var S: String; var Success: Boolean;
  Count: Integer; Line: Boolean);
//標準入力
begin
  if Assigned(FOnStdin) then
    FOnStdin(Self,S,Success,Count,Line);
end;

procedure TDMonkey.SetOnStdin(const Value: TReadStringEvent);
begin
  FOnStdin := Value;
  if Assigned(Value) then
    FEngine.OnStdin := EngineOnStdin
  else
    FEngine.OnStdin := nil;
end;

function TDMonkey.GetLineNumber: Integer;
begin
  Result := FEngine.LineNo;
end;

procedure TDMonkey.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;
  if Assigned(Value) then
    FEngine.OnError := EngineOnError
  else
    FEngine.OnError := nil;
end;

procedure TDMonkey.EngineOnError(Sender: TObject; LineNo: Integer;
  Msg: String);
begin
  if Assigned(FOnError) then
    FOnError(Self,LineNo,Msg);
end;

procedure TDMonkey.RegistDMSObjects;
//拡張objectをインポートする
begin
{$IFNDEF NO_EXTENSION}
  ecma_extobject.RegisterDMS(FEngine);
{$ENDIF}
{$IFNDEF NO_SOCKET}
  ecma_sockobject.RegisterDMS(FEngine);
{$ENDIF}
{$IFNDEF NO_ACTIVEX}
  ecma_activex.RegisterDMS(FEngine);
{$ENDIF}
{$IFNDEF NO_DYNACALL}
  ecma_dynacall.RegisterDMS(FEngine);
{$ENDIF}
{$IFNDEF NO_GUI} {$IFNDEF CONSOLE}
  ecma_guiobject.RegisterDMS(FEngine);
{$ENDIF}         {$ENDIF}
{$IFNDEF NO_VCL} {$IFNDEF CONSOLE}
  ecma_vcl.RegisterDMS(FEngine);
{$ENDIF}         {$ENDIF}
end;

procedure TDMonkey.SetOnDebugout(const Value: TStringEvent);
begin
  FOnDebugout := Value;
  if Assigned(Value) then
    FEngine.OnDebugout := EngineOnDebug
  else
    FEngine.OnDebugout := nil;
end;

function TDMonkey.GetFilename: String;
begin
  Result := FEngine.ScriptFilename;
end;

procedure TDMonkey.EngineOnDoEvents(Sender: TObject;
  var AbortScript: Boolean);
begin
  if Assigned(FOnDoEvents) then
    FOnDoEvents(Self,AbortScript);
end;

procedure TDMonkey.SetOnDoEvents(const Value: TStepEvent);
begin
  FOnDoEvents := Value;
  if Assigned(Value) then
    FEngine.OnDoEvents := EngineOnDoEvents
  else
    FEngine.OnDoEvents := nil;
end;

function TDMonkey.GetFactory: TJObjectFactory;
begin
  Result := FEngine.Factory;
end;

function TDMonkey.GetRegistVar: TJRegistVarType;
begin
  Result := FEngine.RegistVar;
end;

procedure TDMonkey.SetRegistVar(const Value: TJRegistVarType);
begin
  FEngine.RegistVar := Value;
end;

end.
