unit ecma_engine;

//解析木の実行
//2001/04/10 ~
//by Wolfy

{$DEFINE REFCOUNT_DEBUG}

interface

uses
  windows,classes,sysutils,ecma_type,hashtable,ecma_object,
{$IFNDEF NO_SOCKET}
  ecma_sockobject,
{$ENDIF}
{$IFNDEF NO_ACTIVEX}
  activex,ComObj,ecma_activex,
{$ENDIF}
{$IFNDEF NO_DYNACALL}
  dynamiccall,
{$ENDIF}
{$IFNDEF NO_WSH}
  ecma_wsh,
{$ENDIF}
{$IFNDEF CONSOLE}
  forms,
{$ENDIF}
  ecma_parser;

type
  TJEngine = class(TJBaseEngine)
  private
    FParent: TJBaseDMonkey;
    FParser: TJParser;
    FRootTable: TJRootSymbolTable;
    FCurrentTable: TJLocalSymbolTable;
    FFactory: TJObjectFactory;

    FIsRan: Boolean;
    FAbort: Boolean;
    FIsRunning: Boolean;
    FLineNo: Integer;
    FAllowEvent: Boolean;
    FFilename: String;
    FRegistVar: TJRegistVarType;
    //event
    FOnNewObject: TNewObjectEvent;
    FOnStdout: TStringEvent;
    FOnStderr: TStringEvent;
    FOnRun: TNotifyEvent;
    FOnDone: TNotifyEvent;
    FOnStep: TStepEvent;
    FOnStdin: TReadStringEvent;
    FOnError: TErrorEvent;
    FOnDoEvents: TStepEvent;

    //global object
    FGlobalObject: TJGlobalObject;
    FRegExpObject: TJRegExpObject;
{$IFNDEF NO_WSH}
    FWScriptObject: TJWScriptObject;
{$ENDIF}
    procedure Println(S: String);
    procedure PrintlnError(S: String);

    function EvalExpr(P: PJExpr; Flags: TJEvalExprFlags = []): TJValue;
    procedure EvalStatement(P: PJStatement; Flags: TJEvalStatementFlags; SwitchValue: PJSwitchValue = nil);

    function MemberExpr(Parent: TJValue; Member: String; Deleting: Boolean = False): TJValue;
    procedure MemberAssign(Parent: TJValue; Member: String; Value: TJValue);
    function CallArrayExpr(Parent: TJValue; Arguments: PJExpr): TJValue;
    procedure CallArrayAssign(Parent: TJValue; Arguments: PJExpr; Value: TJValue);
    function MethodExpr(P: PJExpr): TJValue;
    procedure MethodAssign(P: PJExpr; Value: TJValue);

    function ArgumentsToList(Arg: PJExpr): TJValueList;
    function ArgumentsToValue(Arg: PJExpr): TJValue;
    function ArgumentsCount(Arg: PJExpr): Integer;
    procedure ObjectExpr(Obj: TJObject; Elements: PJExpr);

    procedure MakeInstance(Obj: TJObject; Members: PJStatement);
    procedure RegistGlobalObjects(ASymbolTable: TJLocalSymbolTable);
    procedure ClearGlobalObjects;

    function GetThisFromValue(Value: TJValue): TJObject;

    procedure BeforeRun(Main: Boolean = False);
    procedure AfterRun;

    //object event
    procedure RegExpOnMatchStart(Sender: TObject);
    procedure RegExpOnMatchEnd(Sender: TObject);
    procedure RegExpOnExecInput(Sender: TObject; var Input: String);

    procedure FactoryOnNewObject(Sender: TObject; JObject: TJObject);

    procedure GlobalObjectOnPrint(Sender: TObject; S: String);
    procedure GlobalObjectOnPrintError(Sender: TObject; S: String);
    procedure GlobalObjectOnRead(Sender: TObject; var S: String; var Success: Boolean;
      Count: Integer; Line: Boolean);

    function GetObjectCount: Integer;
    function GetOnDebugout: TStringEvent;
    procedure SetOnDebugout(const Value: TStringEvent);
  public
    constructor Create(AParent: TJBaseDMonkey);
    destructor Destroy; override;
    procedure Clear;
    procedure Abort;

    function Compile(SourceCode: String): Boolean;
    function CompileFile(AFilename: String; UseBinary: Boolean): Boolean;
    function Run(Root: PJStatement; Args: TJValueList): Integer; overload;
    function Run: Integer; overload;
    function CallExpr(Func: IJFunction; Param: TJValueList; This: TJObject = nil): TJValue;
    function CallFunction(Root: PJStatement; Symbol: String;
      Param: TJValueList; var RetValue: TJValue): Boolean; overload;
    function CallFunction(Symbol: String;
      Param: TJValueList; var RetValue: TJValue): Boolean; overload;
    function CallEvent(var Event: TJValue; Param: TJValueList; This: TJObject = nil): TJValue;
    function Eval(SourceCode: String; This: TJObject = nil): TJValue;

    function MakeObject(Name: String; Param: TJValueList): TJObject; override;
    procedure ImportObject(ObjectName: String; ObjectClass: TJObjectClass); override;
    function IsRunning: Boolean;
    function GetVariable(Symbol: String; var RetVal: TJValue): Boolean;
    function DoEvents: Boolean;
    function GetScriptFilename: String; override;
    function FindImportFilename(Filename: String; var FindedFilename: String): Boolean; override;

    property GlobalObject: TJGlobalObject read FGlobalObject;
    property ObjectCount: Integer read GetObjectCount;
    property Factory: TJObjectFactory read FFactory;
    property Parent: TJBaseDMonkey read FParent;
    property LineNo: Integer read FLineNo write FLineNo;
    property AllowEvent: Boolean read FAllowEvent;
    property ScriptFilename: String read GetScriptFilename;
    property Parser: TJParser read FParser;
    property CurrentTable: TJLocalSymbolTable read FCurrentTable;
    property RegistVar: TJRegistVarType read FRegistVar write FRegistVar;
    //event
    property OnNewObject: TNewObjectEvent read FOnNewObject write FOnNewObject;
    property OnStdout: TStringEvent read FOnStdout write FOnStdout;
    property OnStderr: TStringEvent read FOnStderr write FOnStderr;
    property OnRun: TNotifyEvent read FOnRun write FOnRun;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnStep: TStepEvent read FOnStep write FOnStep;
    property OnStdin: TReadStringEvent read FOnStdin write FOnStdin;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnDebugout: TStringEvent read GetOnDebugout write SetOnDebugout;
    property OnDoEvents: TStepEvent read FOnDoEvents write FOnDoEvents;
  end;

implementation


{ TJEngine }

procedure TJEngine.Println(S: String);
//stdout
begin
  GlobalObjectOnPrint(Self,S + CRLF);
end;

constructor TJEngine.Create(AParent: TJBaseDMonkey);
//作成
begin
  inherited Create;
  FParent := AParent;
  FParser := TJParser.Create;
  FFactory := TJObjectFactory.Create(Self);
  FFactory.OnNewObject := FactoryOnNewObject;
  FRootTable := TJRootSymbolTable.Create(nil);
  FCurrentTable := FRootTable;

  //組込みオブジェクトを登録
  ImportObject('Object',TJObject);
  ImportObject('Global',TJGlobalObject);
  ImportObject('Array',TJArrayObject);
  ImportObject('String',TJStringObject);
  ImportObject('Number',TJNumberObject);
  ImportObject('Boolean',TJBooleanObject);
  ImportObject('RegExp',TJRegExpObject);
  ImportObject('Math',TJMathObject);
  ImportObject('Date',TJDateObject);

  //デフォルトオブジェクト
  FGlobalObject := TJGlobalObject.Create(Self,nil,False);
  FGlobalObject.OnPrint := GlobalObjectOnPrint;
  FGlobalObject.OnRead := GlobalObjectOnRead;
  FRegExpObject := TJRegExpObject.Create(Self,nil,False);
{$IFNDEF NO_WSH}
  FWScriptObject := TJWScriptObject.Create(Self,nil,False);
  FWScriptObject.OnStdOut := GlobalObjectOnPrint;
  FWScriptObject.OnStdErr := GlobalObjectOnPrintError;
  FWScriptObject.OnStdIn := GlobalObjectOnRead;
{$ENDIF}
  //参照カウント
  FGlobalObject.IncRef;
  FRegExpObject.IncRef;
{$IFNDEF NO_WSH}
  FWScriptObject.IncRef;
{$ENDIF}
end;

destructor TJEngine.Destroy;
//破棄
begin
  Clear;
  //参照カウント
  FreeAndNil(FGlobalObject);
  FreeAndNil(FRegExpObject);
{$IFNDEF NO_WSH}
  FreeAndNil(FWScriptObject);
{$ENDIF}

  FreeAndNil(FRootTable);
  FreeAndNil(FFactory);
  FreeAndNil(FParser);
  inherited;
end;

function TJEngine.Run(Root: PJStatement; Args: TJValueList): Integer;
//実行する
begin
  Result := 0;
  //実行中の場合終わり
  if IsRunning then
    Exit;
  //rootが無い場合
  if not Assigned(Root) then
    Root := FParser.Root;
  //さらにrootが無ければ終わり
  if not Assigned(Root) then
    Exit;

  //実行中
  FIsRunning := True;
  try
    Clear;
    FAbort := False;
    FAllowEvent := True;
    //Globalをセット
    FGlobalObject.arguments.Items.Assign(Args);
    //terminatedをセット
    FGlobalObject.Terminate;
    BeforeRun(True);
    try
      //実行
      FCurrentTable := FRootTable;
      EvalStatement(Root,[]);
      //正常終了
      FIsRan := True;
      //生存オブジェクトを使用するためにcurrenttableを__MAIN__に変更する
      FCurrentTable := FRootTable.FindGlobalTable(__MAIN__);
    except
      //Exit,Returnは正常終了扱い
      on E:EJExit do
      begin
        FIsRan := True;
        Result := E.Status;
      end;
      on E:EJReturn do
      begin
        FIsRan := True;
        Result := AsInteger(@E.Value);
      end;
      //例外、中止のときはイベントループに入らない
      on E:EJThrow do
      begin
        FAllowEvent := False;
        PrintlnError('Exception: ' +
          E.ExceptName + '(' + IntToStr(FLineNo) + ') => ' + E.Message);
      end;
      on E:EJAbort do
      begin
        FAllowEvent := False;
        PrintlnError('Abort Script(' + IntToStr(FLineNo) + ')');
      end;
      on E:EJSyntaxError do
      begin
        FAllowEvent := False;
        PrintlnError('SyntaxError: ' +
          'Line(' + IntToStr(FLineNo) + ') => ' + E.Message);
      end;
    end;

    //イベントループに入る
    while ((not FGlobalObject.Terminated) and DoEvents) do
    begin
      Sleep(5);
    end;

    AfterRun;
  finally
    FIsRunning := False;
  end;
end;

procedure TJEngine.EvalStatement(P: PJStatement;
  Flags: TJEvalStatementFlags; SwitchValue: PJSwitchValue);
//文を順番に実行する
var
  current,catch: PJStatement;
  v,compared: TJValue;
  abrt,ebreak: Boolean;
  element: String;
  sl: TStringList;
  i: Integer;
  func: IJFunction;
  switch: TJSwitchValue;
{$IFNDEF NO_ACTIVEX}
  enum: TJEnumeratorObject;
  param: TJValueList;
{$ENDIF}
  arry: TJObject;
  blocktable,
  oldtable: TJLocalSymbolTable;
begin
  current := P;
  while Assigned(current) do
  begin
    //abort
    if FAbort then
      raise EJAbort.Create('script abort');

    //行番号
    FLineNo := current^.LineNo;
    //イベント
    if Assigned(FOnStep) then
    begin
      abrt := False;
      FOnStep(Self,abrt);
      //中止する
      if abrt then
        raise EJAbort.Create('script abort');
    end;

    case current^.SType of
      stNone:;

      //sourceインポート
      stSource:
      begin
        //Rootテーブルに値として登録
        EmptyFunction(func);
        func.FuncType := ftImport;
        func.vStatement := current^.Sub1;
        //名前空間 exprがない時はGlobal
        if not Assigned(current^.Expr) then
          func.Symbol := __MAIN__
        else
          func.Symbol := current^.Expr^.Symbol;

        //テーブルを作成 pointerを渡す thisを登録
        func.FunctionTable := FRootTable.MakeGlobalTable(func.Symbol,current);
        func.FunctionTable.This := FGlobalObject;
        //グローバル変数を登録
        RegistGlobalObjects(func.FunctionTable);
        //Namespaceを登録
        FRootTable.RegistGlobalValue(func.Symbol,BuildFunction(func));
        //新しいテーブルで実行
        oldtable := FCurrentTable;
        FCurrentTable := func.FunctionTable;
        try
          EvalStatement(func.vStatement,[]);
        finally
          FCurrentTable := oldtable;
          //global tableは削除しない
        end;
      end;
      //変数宣言開始
      stVar:
      begin
        //stVarDeclへ
        EvalStatement(current^.Sub1,Flags + [esfVar]);
      end;
      //static変数宣言開始
      stStatic:
      begin
        //stVarDeclへ
        EvalStatement(current^.Sub1,Flags + [esfStaticVar]);
      end;
      //Global変数宣言開始
      stGlobal:
      begin
        //stVarDeclへ
        EvalStatement(current^.Sub1,Flags + [esfGlobalVar]);
      end;
      stVariableDecl:
      begin  //変数宣言登録
        if Assigned(current^.Expr^.Left) then
          v := EvalExpr(current^.Expr^.Left)
        else  //0を入れる
          v := BuildInteger(0);

        //変数登録
        if esfVar in Flags then
          FCurrentTable.RegistValue(current^.Expr^.Symbol,v)
        else if esfStaticVar in Flags then
          FCurrentTable.RegistStaticValue(current^.Expr^.Symbol,v)
        else
          FCurrentTable.RegistGlobalValue(current^.Expr^.Symbol,v)
      end;
      //関数定義
      stFunctionDecl:
      begin
        //関数をセット
        EmptyFunction(func);
        func.FuncType := ftStatement;
        func.vStatement := current;
        //パラメータセット
        func.Parameter := current^.Sub1;
        //シンボルテーブルを作成
        //親関数から継承する
        func.FunctionTable :=
          FRootTable.GetFunctionTable(current^.Temp,current);
        //変数式が存在しない場合がある
        if Assigned(current^.Expr) then
        begin
          //関数名
          func.Symbol := current^.Expr^.Symbol;
          //テーブルに登録する
          FCurrentTable.RegistValue(func.Symbol,BuildFunction(func));
        end;
      end;
      //クラス定義
      stClassDecl:
      begin
        EmptyFunction(func);
        func.Symbol := current^.Expr^.Symbol;
        func.FuncType := ftClass;
        func.vStatement := current;
        //シンボルテーブルを作成
        //親関数から継承する
        func.FunctionTable :=
          FRootTable.GetFunctionTable(current^.Temp,current);
        //登録
        FCurrentTable.RegistValue(func.Symbol,BuildFunction(func));
      end;
      //式文
      stExpr: EvalExpr(current^.Expr);
      //blockを実行
      stBlock:
      begin
        EvalStatement(current^.Sub1,Flags);
      end;
      //for文
      stFor:
      begin
        //初期化文
        EvalExpr(current^.Expr);
        while True do
        begin
          //条件式が存在する場合のみ
          if Assigned(current^.Sub2^.Expr) then
          begin
            v := EvalExpr(current^.Sub2^.Expr);
            if not AsBool(@v) then
              Break;
          end;

          ebreak := False;
          try
            try
              //ブロックを実行
              EvalStatement(current^.Sub1,[esfIteration]);
            except
              //例外として実装する
              on EJBreak do
              begin
                ebreak := True;
                Break;
              end;
              on EJContinue do
                Continue;
            end;
          finally
            //後始末文 breakの場合は実行しない
            if not ebreak then
              EvalExpr(current^.Sub2^.Next^.Expr);
          end;
        end;
      end;
      //if文
      stIf:
      begin
        v := EvalExpr(current^.Expr);
        if AsBool(@v) then
          EvalStatement(current^.Sub1,Flags)
        else begin
          EvalStatement(current^.Sub2,Flags);
        end;
      end;
      //while 文
      stWhile:
      begin
        while True do
        begin
          v := EvalExpr(current^.Expr);
          if not AsBool(@v) then
            Break;

          try
            //文を実行
            EvalStatement(current^.Sub1,[esfIteration]);
          except
            //例外として実装する
            on EJBreak do
              Break;
            on EJContinue do
              Continue;
          end;
        end;
      end;
      //for in文
      stForIn,stForInArrayElement:
      begin
        //var無し
        if current^.Expr^.Code = opVariable then
          element := current^.Expr^.Symbol
        //varあり
        else if Assigned(current^.Expr^.Left) and
               (current^.Expr^.Left^.Code = opVariable) then
        begin
          element := current^.Expr^.Left^.Symbol;
          //ローカルに変数を登録
          EmptyValue(v);
          FCurrentTable.RegistValue(element,v);
        end
        else //変数で無い場合は例外
          raise EJThrow.Create(E_TYPE,'need variable - for..in');

        v := EvalExpr(current^.Sub2^.Expr);
        //配列
        if IsArrayObject(@v) then
        begin
          arry := v.vObject;
          case current^.SType of
            //要素を入れる
            stForInArrayElement:
            begin
              arry.IncRef;
              try
                for i := 0 to arry.GetCount - 1 do
                begin
                  FCurrentTable.SetValue(element,arry.GetItem(i),FRegistVar);
                  try
                    //ブロックを実行
                    EvalStatement(current^.Sub1,[esfIteration]);
                  except
                    //例外として実装する
                    on EJBreak do
                      Break;
                    on EJContinue do
                      Continue;
                  end;
                end;
              finally
                arry.DecRef;
              end;
            end;
            //indexを入れる
            stForIn:
            begin
              for i := 0 to arry.GetCount - 1 do
              begin
                FCurrentTable.SetValue(element,BuildInteger(i),FRegistVar);
                try
                  //ブロックを実行
                  EvalStatement(current^.Sub1,[esfIteration]);
                except
                  //例外として実装する
                  on EJBreak do
                    Break;
                  on EJContinue do
                    Continue;
                end;
              end;
            end;
          end;
        end
{$IFNDEF NO_ACTIVEX}
        else if IsCollection(@v) then //Enumerator実行
        begin
          param := TJValueList.Create;
          try
            param.Add(v);
            enum := TJEnumeratorObject.Create(Self,param);
            try
              enum.IncRef;

              while not enum.AtEnd do
              begin
                FCurrentTable.SetValue(element,enum.Item,FRegistVar);
                try try
                  //ブロックを実行
                  EvalStatement(current^.Sub1,[esfIteration]);
                finally
                  enum.MoveNext;
                end;
                except
                  //例外として実装する
                  on EJBreak do
                    Break;
                  on EJContinue do
                    Continue;
                end;
              end;
            finally
              enum.DecRef;
            end;
          finally
            param.Free;
          end;
        end
{$ENDIF}
        else if IsObject(@v) then
        begin
          //全てのkeyを得る
          sl := TStringList.Create;
          try
            v.vObject.GetPropertyList(sl);
            for i := 0 to sl.Count - 1 do
            begin
              //keyを変数に入れる
              FCurrentTable.SetValue(element,BuildString(sl[i]),FRegistVar);
              try
                //ブロックを実行
                EvalStatement(current^.Sub1,[esfIteration]);
              except
                //例外として実装する
                on EJBreak do
                  Break;
                on EJContinue do
                  Continue;
              end;
            end;
          finally
            sl.Free;
          end;
        end
        else //オブジェクトでないなら例外
          raise EJThrow.Create(E_TYPE,'need object,array or collection - for..in');
      end;

      //switch文
      stSwitch:
      begin
        //式を評価
        v := EvalExpr(current^.Expr);
        try
          //labeled文を実行する SwitchValue付き
          switch.Match := False;
          switch.Default := nil;
          switch.Value := @v;
          //iterationをtrue
          //まず一度default句を無視して実行してみる
          EvalStatement(current^.Sub1,[esfIteration],@switch);
          //matchがなくてdefault句が存在すればdefault句から実行
          if not switch.Match and Assigned(switch.Default) then
          begin
            switch.Match := True;
            EvalStatement(switch.Default,[esfIteration],@switch);
          end;
        except //breakを受ける
          on EJBreak do
        end;
      end;
      //case default文
      stLabeled:
      begin
        //switch valueがある時のみ実行
        if Assigned(SwitchValue) then
        begin
          //matchしている時は無条件で実行する
          if SwitchValue^.Match then
            EvalStatement(current^.Sub1,Flags,nil)
          else begin
            //case:
            if Assigned(current^.Expr) then
            begin
              //定数値を得る
              v := EvalExpr(current^.Expr);
              //正規表現リテラルで比較する
              if IsRegExpObject(@v) then
              begin
                compared :=
                  BuildBool((v.vObject as TJRegExpObject).Test(SwitchValue^.Value^));
              end
              else if IsRegExp(@v) then
              begin
                FRegExpObject.SetRegExpValue(v);
                compared :=
                  BuildBool(FRegExpObject.Test(SwitchValue^.Value^));
              end
              else //等式で比較する
                compared := CompareValue(opEQ,v,SwitchValue^.Value^);

              if AsBool(@compared) then
              begin
                EvalStatement(current^.Sub1,Flags,nil);
                //一致したら以降は無条件で実行
                SwitchValue^.Match := True;
              end;
            end
            //default:
            else
              SwitchValue^.Default := current; //default句の場所を保存しとく
          end;
        end
        else //変な場所にlabelがあった
          raise EJThrow.Create(E_SYNTAX,'switch case: or default:');
      end;

      stDo: //do - while文
      begin
        while True do
        begin
          try
            //文を実行
            EvalStatement(current^.Sub1,[esfIteration]);
          except
            //例外として実装する
            on EJBreak do
              Break;
            on EJContinue do
              Continue;
          end;

          //式を判断
          v := EvalExpr(current^.Expr);
          if not AsBool(@v) then
            Break;
        end;
      end;
      //break文  for,while doとswitchを止める
      stBreak:
      begin
        if esfIteration in Flags then
          raise EJBreak.Create('break')
        else
          raise EJThrow.Create(E_SYNTAX,'break');
      end;
      //continue文
      stContinue:
      begin
        if esfIteration in Flags then
          raise EJContinue.Create('continue')
        else
          raise EJThrow.Create(E_SYNTAX,'continue');
      end;
      //return文
      stReturn:
      begin
        //return例外
        v := EvalExpr(current^.Expr);
        //参照カウントを一つ増やす
        if IsObject(@v) then
          v.vObject.IncRef
        //関数ならば変数をコピー
        else if IsFunction(@v) then
        begin
          //新規作成
          EmptyFunction(func);
          //copy
          func.Assign(v.vFunction);
          //localtable作成
          if not Assigned(func.LocalTable) then
            func.LocalTable := TJLocalSymbolTable.Create(nil);

          //カレント変数をcopy
          func.LocalTable.LocalCopy(FCurrentTable);
          //vを変更
          v := BuildFunction(func);
        end;

        raise EJReturn.Create(v);
      end;
      //throw文
      stThrow:
      begin
        v := EvalExpr(current^.Expr);
        //参照カウントを一つ増やす
        if IsObject(@v) then
          v.vObject.IncRef
        else if IsUndefined(@v) then
          v := BuildString(''); //空文字列
        //throw例外
        raise EJThrow.Create(E_THROW,'',@v);
      end;
      //try文
      stTry:
      begin
        //try - sub1(block)
        //    - sub2(catch)
        //    - sub3(finally)
        try try
          EvalStatement(current^.Sub1,Flags);
        except
          //catch文
          on E:EJRuntimeError do
          begin
            //再生成
            if not Assigned(current^.Sub2) then
              raise;

            catch := current^.Sub2;
            //変数登録
            if IsVariable(catch^.Expr) then
            begin
              //user例外
              if E.ExceptName = E_THROW then
              begin
                //tempに参照カウントを変化させないで登録
                if IsObject(@E.Value) then
                  FCurrentTable.AddTemporaryObject(E.Value.vObject,False);
                //登録
                FCurrentTable.RegistValue(catch^.Expr^.Symbol,E.Value);
              end
              else //例外名を登録
                FCurrentTable.RegistValue(
                  catch^.Expr^.Symbol,BuildString(E.ExceptName));
            end;
            //catch文実行
            EvalStatement(catch^.Sub1,Flags);
          end;
        end;

        finally
          //finally文実行
          if Assigned(current^.Temp) then
            EvalStatement(current^.Temp^.Sub1,Flags);
        end;
      end;
      //with文
      stWith:
      begin
        v := EvalExpr(current^.Expr);
        //objectでないなら例外
        if not IsObject(@v) then
          raise EJThrow.Create(E_TYPE,'need object - with');

        //新しいブロックを作る thisをpush
        blocktable := FCurrentTable.PushLocalTable(nil,v.vObject);
        oldtable := FCurrentTable;
        FCurrentTable := blocktable;
        try
          EvalStatement(current^.Sub1,Flags);
        finally
          FCurrentTable := oldtable;
          FCurrentTable.PopLocalTable;
        end;
      end;

    end;

    //temp objectをクリア
    FCurrentTable.ClearTemporaryObject;
    //次へ
    current := current^.Next;
  end;
end;

function TJEngine.EvalExpr(P: PJExpr; Flags: TJEvalExprFlags): TJValue;
//式を評価する
var
  l,r,t: PJExpr;
  v: TJValue;
  param: TJValueList;
  name: String;
  func: IJFunction;
begin
  EmptyValue(Result);
  if not Assigned(P) then
    Exit;

  l := P^.Left;
  r := P^.Right;
  t := P^.Third;

  case P^.Code of
    opExpr:
    begin
      EvalExpr(l);
      Result := EvalExpr(r);
    end;

    //定数…そのまま返す
    opConstant:
    begin
      Result := P^.Value^;
    end;

    //ローカル変数宣言(forで使われる)
    opVar:
    begin
      //次へ
      Result := EvalExpr(l,Flags + [eefVar]);
    end;

    //変数…テーブルから検索する
    opVariable:
    begin
      //変数宣言
      {if eefVar in Flags then
      //  FCurrentTable.RegistLocal(P^.Symbol,Result)
      //通常
      else
      }
      if FCurrentTable.GetValue(P^.Symbol,Result) then
      begin
        {if eefDelete in Flags then
        begin
        end;}
      end
      else //変数が未定義なので例外
        raise EJThrow.Create(E_NAME,'undefined - ' + P^.Symbol);
    end;

    //代入式 variable = expr
    opAssign:
    begin
      Result := EvalExpr(r);
      //代入できるのは4種類のみ
      case l^.Code of
        opVariable:
        begin
          if eefVar in Flags then //opVar(for文)で使用
            FCurrentTable.RegistValue(l^.Symbol,Result)
          else
            FCurrentTable.SetValue(l^.Symbol,Result,FRegistVar);
        end;

        opMember: MemberAssign(EvalExpr(l^.Left),l^.Right^.Symbol,Result);
        opCallArray: CallArrayAssign(EvalExpr(l^.Left),l^.Right,Result);
        opMethod: MethodAssign(l,Result);
      else
        raise EJThrow.Create(E_TYPE,'can not assign - ' + l^.Symbol);
      end;
    end;

    //演算代入式
    opMulAssign,opDivAssign,opAddAssign,opSubAssign,opModAssign,
    opBitLeftAssign,opBitRightAssign,opBitRightZeroAssign,
    opBitAndAssign,opBitXorAssign,opBitOrAssign:
    begin
      Result := AssignValue(P^.Code,EvalExpr(l),EvalExpr(r));
      //代入できるのは4種類のみ
      case l^.Code of
        opVariable: FCurrentTable.SetValue(l^.Symbol,Result,FRegistVar);
        opMember: MemberAssign(EvalExpr(l^.Left),l^.Right^.Symbol,Result);
        opCallArray: CallArrayAssign(EvalExpr(l^.Left),l^.Right,Result);
        opMethod: MethodAssign(l,Result);
      else
        raise EJThrow.Create(E_TYPE,'can not assign - ' + l^.Symbol);
      end;
    end;

    //メンバ呼び出し L ... object  R ... variable
    opMember: Result := MemberExpr(EvalExpr(l),r^.Symbol,eefDelete in Flags);

    //配列           L ... object|function  R ... arguments
    opCallArray: Result := CallArrayExpr(EvalExpr(l),r);

    //メソッド L..object R..variable T..arg
    opMethod: Result := MethodExpr(P);

    opMinus,opPlus,opBitNot:
    begin
      Result := CalcValue1(P^.Code,EvalExpr(l));
    end;
    opPreInc:
    begin
      Result := EvalExpr(l);
      Result := BuildInteger(AsInteger(@Result) + 1);
      if l^.Code = opVariable then
        FCurrentTable.SetValue(l^.Symbol,Result,FRegistVar);
    end;
    opPreDec:
    begin
      Result := EvalExpr(l);
      Result := BuildInteger(AsInteger(@Result) - 1);
      if l^.Code = opVariable then
        FCurrentTable.SetValue(l^.Symbol,Result,FRegistVar);
    end;
    opPostInc:
    begin
      Result := EvalExpr(l);
      if l^.Code = opVariable then
        FCurrentTable.SetValue(l^.Symbol,BuildInteger(AsInteger(@Result) + 1),FRegistVar);
    end;
    opPostDec:
    begin
      Result := EvalExpr(l);
      if l^.Code = opVariable then
        FCurrentTable.SetValue(l^.Symbol,BuildInteger(AsInteger(@Result) - 1),FRegistVar);
    end;
    //２次式
    opAdd,opSub,opMul,opDiv,opMod,opDivInt,opBitAnd,opBitOr,opBitXor,
    opBitLeft,opBitRight,opBitRightZero:
    begin
      Result := CalcValue2(P^.Code,EvalExpr(l),EvalExpr(r));
    end;

    //比較
    opLS,opGT,opLSEQ,opGTEQ,opEQ,opNE,opEQEQEQ,opNEEQEQ,
    opLogicalOr2,opLogicalAnd2:
    begin
      Result := CompareValue(P^.Code,EvalExpr(l),EvalExpr(r));
    end;
    //ショートサーキット評価
    opLogicalOr:
    begin
      v := EvalExpr(l);
      if AsBool(@v) then
        Result := BuildBool(True) //rightは評価しない
      else begin
        v := EvalExpr(r);
        Result := BuildBool(AsBool(@v));
      end;
    end;
    opLogicalAnd:
    begin
      v := EvalExpr(l);
      if not AsBool(@v) then
        Result := BuildBool(False) //rightは評価しない
      else begin
        v := EvalExpr(r);
        Result := BuildBool(AsBool(@v));
      end;
    end;
    opLogicalNot:
    begin
      Result := CompareValue(P^.Code,EvalExpr(l),v);
    end;
    //３次式
    opConditional:
    begin
      v := EvalExpr(l);
      if AsBool(@v) then //片方のみ評価する
        Result := EvalExpr(r)
      else
        Result := EvalExpr(t);
      //Result := CalcValue3(opConditional,Evalexpr(l),EvalExpr(r),EvalExpr(t));
    end;
    //関数式
    opFunction:
    begin
      //関数をセット
      EmptyFunction(func);
      func.FuncType := ftStatement;
      func.vStatement := P^.Statement;
      //パラメータセット
      func.Parameter := P^.Statement^.Sub1;
      //名前
      if Assigned(P^.Statement^.Expr) then
        func.Symbol := P^.Statement^.Expr^.Symbol;

      //シンボルテーブルを作成
      //親関数から継承する
      func.FunctionTable :=
        FRootTable.GetFunctionTable(P^.Statement^.Temp,P^.Statement);
      {//localtable作成
      if not Assigned(func.LocalTable) then
        func.LocalTable := TJLocalSymbolTable.Create(nil);
      //カレント変数をcopy
      func.LocalTable.LocalCopy(FCurrentTable);
      }
      Result := BuildFunction(func);
    end;

    //object作成
    opNew:      //L..Object名 R..引数
    begin
      name := l^.Symbol;
      param := ArgumentsToList(r);
      try
        //object作成
        Result := BuildObject(MakeObject(name,param));
      finally
        param.Free;
      end;

      //tempに入れる
      //FCurrentTable.AddTemporaryObject(Result.vObject);
    end;
    //Object作成
    opNewObject:
    begin
      Result := BuildObject(TJObject.Create(Self));
      ObjectExpr(Result.vObject,l);
      //tempに入れる
      FCurrentTable.AddTemporaryObject(Result.vObject);
    end;
    //配列作成
    opNewArray:
    begin
      param := ArgumentsToList(l);
      try
        //初期値セット
        Result.ValueType := vtObject;
        if IsParam1(param) and (param.Count = 1) then //引数がひとつのときも常に要素として扱う
        begin
          Result.vObject := TJArrayObject.Create(Self);
          (Result.vObject as TJArrayObject).Add(param[0]);
        end
        else
          Result.vObject := TJArrayObject.Create(Self,param);
      finally
        param.Free;
      end;

      //tempに入れる
      FCurrentTable.AddTemporaryObject(Result.vObject);
    end;
    //現在のカレントobjectを返す
    opThis:
    begin
      Result := BuildObject(FCurrentTable.This);
    end;
    opSuper:
    begin
      raise EJThrow.Create(E_SYNTAX,'super');
      //Result := MemberExpr(P,FTables.This);
      //Result.ValueType := vtObject;
      //Result.vObject := FTable.This;
    end;
    opDelete:
    begin
      //削除する
      Result := EvalExpr(l,[eefDelete]);
    end;
    opVoid:
    begin
      Result := Evalexpr(l);
      Result := BuildNull;
    end;
    opTypeOf:
    begin
      try
        v := EvalExpr(l);
      except
        on E:EJThrow do
        begin
          //undefine
          if (E.ExceptName = E_NAME) or (E.ExceptName = E_KEY) then
            EmptyValue(v)
          else //再生成
            raise;
        end;
      end;
      Result := BuildString(TypeOf(@v));
    end;
    else begin
      EvalExpr(l);
      EvalExpr(r);
    end;
  end;

end;

function TJEngine.CallFunction(Root: PJStatement; Symbol: String;
  Param: TJValueList; var RetValue: TJValue): Boolean;
//外部からの関数呼び出し
var
  v: TJValue;
  old,table: TJLocalSymbolTable;
begin
  Result := False;
  EmptyValue(RetValue);
  //実行中の場合終わり
  if IsRunning then
    Exit;
  //実行済みでない場合
  if not FIsRan then
    Exit;
  //実行中
  FIsRunning := True;

  try
    FAbort := False;
    //rootがあれば実行
    //if Assigned(Root) then
    //  Run(Root,nil);

    //関数を実行する
    table := FRootTable.FindGlobalTable(__MAIN__);
    if table.GetValue(Symbol,v) and
       IsFunction(@v) then
    begin
      old := FCurrentTable;
      FCurrentTable := table;
      BeforeRun;
      try try
        RetValue := CallExpr(v.vFunction,Param);
        Result := True;
      except
        on E:EJThrow do
          PrintlnError('Exception: ' +
            E.ExceptName + '(' + IntToStr(FLineNo) + ') => ' + E.Message);
        on E:EJAbort do
          PrintlnError('Abort Script(' + IntToStr(FLineNo) + ')');
        on E:EJSyntaxError do
          PrintlnError('SyntaxError: ' +
            'Line(' + IntToStr(FLineNo) + ') => ' + E.Message);
      end;

      finally
        AfterRun;
        FCurrentTable := old;
      end;
    end;
  finally
    FIsRunning := False;
  end;
end;

function TJEngine.CallExpr(Func: IJFunction; Param: TJValueList;
  This: TJObject): TJValue;
//関数呼び出し
var
  paramdecl: PJStatement;
  i,index: Integer;
  args: TJArrayObject;
  table,oldtable: TJLocalSymbolTable;
  v: TJValue;
{$IFNDEF NO_ACTIVEX}
  oleret: OleVariant;
  dispparams: TDispParams;
  arglist: PVariantArgList;
  diput: TDispId;
{$ENDIF}
{$IFNDEF NO_DYNACALL}
  dynavalues: TDynaValueArray;
{$ENDIF}
begin
  EmptyValue(Result);
{$IFNDEF NO_DYNACALL}
  dynavalues := nil;
{$ENDIF}

  try
    case Func.FuncType of
      //構文木
      ftStatement:
      begin
        //※Paramの値は削除したりしない

        //method ownerをチェック
        if Assigned(Func.MethodOwner) then
          This := Func.MethodOwner;

        //テーブル作成
        table := Func.FunctionTable.PushLocalTable(Func.LocalTable,This);
        oldtable := FCurrentTable;
        FCurrentTable := table;
        try
          //flagがfcfCall fcfApplyの場合はParam[0]がthisになる
          if (Func.Flag = fcfCall) and IsParam1(Param) then
          begin
            //argumentsを作成
            args := TJArrayObject.Create(Self);

            table.This := GetThisFromValue(Param[0]);

            for i := 1 to Param.Count - 1 do
              args.Add(Param[i]);
          end
          //applyの場合はParam[0]がthis Param[1]がarguments
          else if (Func.Flag = fcfApply) and IsParam1(Param) then
          begin
            table.This := GetThisFromValue(Param[0]);

            if IsParam2(Param) then
            begin
              v := Param[1];
              if IsObject(@v) and (v.vObject is TJArrayObject) then
              begin
                //Paramを入れ替え
                args := v.vObject as TJArrayObject;
              end
              else
                raise EJThrow.Create(E_TYPE,Func.Symbol + '.apply arguments error');
            end
            else //argumentsを作成
              args := TJArrayObject.Create(Self);
          end
          else begin
            //通常
            //argumentsを作成
            args := TJArrayObject.Create(Self);

            if IsParam1(Param) then
              for i := 0 to Param.Count - 1 do
                args.Add(Param[i]);
          end;

          //argumentsにcalleeをつける
          EmptyValue(v);
          v.ValueType := vtFunction;
          v.vFunction := Func;
          //配列はarraystyle=falseで登録しないとエラー
          args.SetValue('callee',v,False);
          //arguments登録
          table.RegistValue('arguments',BuildObject(args));
          //パラメータ登録
          i := 0;
          paramdecl := Func.Parameter;
          while Assigned(paramdecl) do
          begin
            EmptyValue(v);
            //v := BuildNull;
            //順番にローカル変数に登録
            if Assigned(paramdecl^.Expr) then
            begin
              if i < args.Count then
                table.RegistValue(paramdecl^.Expr^.Symbol,args.GetItem(i))
              else //undifinedを登録
                table.RegistValue(paramdecl^.Expr^.Symbol,v);
            end;

            paramdecl := paramdecl^.Next;
            Inc(i);
          end;

          try
            EvalStatement(Func.vStatement^.Sub2,[]);
          except
            on E:EJReturn do
              Result := E.Value;
          end;
        finally
          //テーブルを削除
          Func.FunctionTable.PopLocalTable;
          //元に戻す
          FCurrentTable := oldtable;
        end;
      end;
      //Delphiメソッド
      ftMethod: Result := Func.vMethod(Param);

  {$IFNDEF NO_ACTIVEX}
      //ActiveXメソッド
      ftActiveX:
      begin
        //VarClear(oleret); VarClearはバグっている
        VariantInit(oleret);
        //パラメータ作成
        if IsParam1(Param) then
        begin
          GetMem(arglist,SizeOf(TVariantArg) * Param.Count);
          //逆順にする
          index := 0;
          for i := Param.Count - 1 downto 0 do
          begin
            //tagVariantとOleVariantは同じ
            arglist^[index] := TVariantArg(ValueToVariant(Param[i]));
            Inc(Index);
          end;
          dispparams.rgvarg := arglist;
          dispparams.cArgs := Param.Count;
          dispparams.rgdispidNamedArgs := nil;
          dispparams.cNamedArgs := 0;
        end
        else begin
          arglist := nil;
          dispparams.rgvarg := nil;
          dispparams.cArgs := 0;
          dispparams.rgdispidNamedArgs := nil;
          dispparams.cNamedArgs := 0;
        end;

        try
          //property putの場合
          if Func.vActiveX.Flag = axfPut then
          begin
            diput := DISPID_PROPERTYPUT;
            dispparams.rgdispidNamedArgs := @diput;
            dispparams.cNamedArgs := 1;
          end;

          //呼び出し
          try
            //メソッド呼び出しのバグはVarClearをVariantInitに代えると直った
            OleCheck(Func.vActiveX.Parent.Invoke(
              Func.vActiveX.Dispid,
              GUID_NULL,
              GetUserDefaultLCID,
              AXMethodFlagToDisp(Func.vActiveX.Flag),
              dispparams,
              @oleret,nil,nil));

            Result := VariantToValue(oleret,Self);
          except
            //例外
            raise EJThrow.Create(E_ACTIVEX,
              AXMethodFlagToString(Func.vActiveX.Flag) + ' error: ' + Func.Symbol);
          end;
        finally
          if Assigned(arglist) then
            FreeMem(arglist);
        end;
      end;
    {$ENDIF}

    {$IFNDEF NO_DYNACALL}
      //DLL関数の呼び出し
      ftDynaCall:
      begin
        //SynaValueリストを作成する
        dynavalues := ValueListToDynaValueArray(Func.vDynaCall.Arguments,Param);
        //呼び出し
        Result :=
          DynaResultToValue(
            Func.vDynaCall.ReturnValue,
            DynaCall(
              MakeCallFlags(Func.vDynaCall.Call),
              Func.vDynaCall.Procaddr,
              DynaValueArrayToDynaParmArray(dynavalues),
              nil,
              0
            )
          );
        //参照渡しの値を反映する
        SetRefDynaValue(dynavalues,Param);
      end;
    {$ENDIF}
    else
      raise EJThrow.Create(E_CALL,'not support function type');
    end;
  finally
    //result objectをtempに登録する
    if IsObject(@Result) then
      FCurrentTable.AddTemporaryObject(
        Result.vObject,
        //ftStatementの時は参照カウントを変化させないのでfalse
        Func.FuncType <> ftStatement);
  end;
end;

function TJEngine.MemberExpr(Parent: TJValue; Member: String; Deleting: Boolean): TJValue;
//メンバ式
var
  ax,vcl: Boolean;
  obj: TJObject;
begin
  EmptyValue(Result);
  ax := False;
  vcl := False;

  if IsObject(@parent) then
  begin
{$IFNDEF NO_ACTIVEX}
    //activexではmember exprで新規objectが帰ってくる
    if (parent.vObject is TJActiveXObject) then
      ax := True
    else
{$ENDIF}
    //vclではmember exprで新規objectが帰ってくる
    if (parent.vObject is TJVCLPersistent) then
      vcl := True;

    //property削除
    if Deleting then
      Result := BuildBool(parent.vObject.RemoveKey(Member))
    //prototype
    else if Member = 'prototype' then
      Result := BuildObject(FFactory.GetPrototype(parent.vObject.Name))
    else
      Result := parent.vObject.GetValue(Member,False);
  end
  else if IsString(@parent) then //文字列のプロパティ
  begin
    obj := MakeObject('String',nil);
    (obj as TJStringObject).text := AsString(@parent);
    Result := obj.GetValue(Member,False);

{ TODO : 特殊文字列らしい }
// "$$" $ という文字そのもの
// "$&" 前回一致した部分です
// "$`" 前回一致した部分より前方の文字列です
// "$'" 前回一致した部分より後方の文字列です
// "$n" 前回一致したn番目(1-9,01-99)の部分です
  end
  else if IsRegExp(@parent) then //正規表現
  begin
    obj := MakeObject('RegExp',nil);
    (obj as TJRegExpObject).SetRegExpValue(parent);
    Result := obj.GetValue(Member,False);
  end
  else if TryAsNumber(@parent) then //数字
  begin
    obj := MakeObject('Number',nil);
    (obj as TJNumberObject).FValue := parent;
    Result := obj.GetValue(Member,False);
  end
  else if IsConstructor(@parent) then //関数
  begin
    //prototype
    if Member = 'prototype' then
      Result := BuildObject(FFactory.GetPrototype(parent.vFunction.Symbol))
    else if Member = 'call' then
    begin
      //コピー
      Result := parent;
      //call flagをセット
      Result.vFunction.Flag := fcfCall;
    end
    else if Member = 'apply' then
    begin
      //コピー
      Result := parent;
      //call flagをセット
      Result.vFunction.Flag := fcfApply;
    end
    else //エラー
      raise EJThrow.Create(E_NAME,'member error ' + Member);
  end
  else if IsBool(@parent) then //bool
  begin
    obj := MakeObject('Boolean',nil);
    (obj as TJBooleanObject).FBool := AsBool(@parent);
    Result := obj.GetValue(Member,False);
  end
{$IFNDEF NO_ACTIVEX}
  else if IsDispatch(@parent) then
  begin
    ax := True;
    obj := MakeObject('ActiveXObject',nil);
    (obj as TJActiveXObject).disp := AsDispatch(@parent);
    Result := obj.GetValue(Member,False);
  end
{$ENDIF}
  else if IsNameSpace(@parent) then
  begin
    //名前空間から
    FRootTable.FindGlobalTable(parent.vFunction.Symbol).GetValue(Member,Result);
  end
  else
    raise EJThrow.Create(E_NAME,'member error ' + Member);
{$IFNDEF NO_ACTIVEX}
  //activexObjectの場合はtempに登録する
  if ax and IsObject(@Result) and (Result.vObject is TJActiveXObject) then
    FCurrentTable.AddTemporaryObject(Result.vObject)
  else
{$ENDIF}
  if vcl and IsVCLObject(@Result) then
    FCurrentTable.AddTemporaryObject(Result.vObject);
end;

procedure TJEngine.MemberAssign(Parent: TJValue; Member: String; Value: TJValue);
//メンバへ代入
//member ... object.variable
begin
  if IsObject(@parent) then
  begin
    //protoype
    if (Member = 'prototype') and (IsObject(@Value) or IsNull(@Value)) then
    begin
      //objectを代入する
      if not FFactory.SetPrototype(parent.vObject.Name,Value.vObject) then
        raise EJThrow.Create(E_NAME,'prototype assign error ' + Value.vObject.Name);
    end
    else
      parent.vObject.SetValue(Member,Value,False);
  end
  //関数 prototype
  else if IsConstructor(@parent) and (Member = 'prototype') and
          (IsObject(@Value) or IsNull(@Value)) then
  begin
    //objectを代入する
    if not FFactory.SetPrototype(parent.vFunction.Symbol,Value.vObject) then
      raise EJThrow.Create(E_NAME,'prototype assign error ' + Value.vObject.Name);
  end
  //名前空間
  else if IsNameSpace(@parent) then
    FRootTable.FindGlobalTable(parent.vFunction.Symbol).SetValue(Member,Value,FRegistVar)
  else
    raise EJThrow.Create(E_NAME,'member assign error ' + Member);
end;

procedure TJEngine.Clear;
//テーブルをクリアする
begin
  FAllowEvent := False;
  FIsRan := False;
  ClearGlobalObjects;
  FRootTable.Clear;
  FFactory.Clear;
  FLineNo := E_UNKNOWN_LINE_NO;
end;

procedure TJEngine.Abort;
begin
  FAbort := True;
  FAllowEvent := False;
end;

procedure TJEngine.FactoryOnNewObject(Sender: TObject;
  JObject: TJObject);
var
  re: TJRegExpObject;
begin
  if JObject is TJRegExpObject then
  begin
    //正規表現に細工
    re := TJRegExpObject(JObject);
    re.OnMatchStart := RegExpOnMatchStart;
    re.OnMatchEnd := RegExpOnMatchEnd;
    re.OnExecInput := RegExpOnExecInput;
  end;

{$IFNDEF NO_SOCKET}
  //socket関係に細工
  if JObject is TJBaseSocketObject then
  begin
    TJBaseSocketObject(JObject).OnPrint := FGlobalObject.Println;
  end;
{$ENDIF}

  if Assigned(FOnNewObject) then
    FOnNewObject(Self,JObject);
end;

procedure TJEngine.ObjectExpr(Obj: TJObject; Elements: PJExpr);
//L(変数)
//R(値)
var
  current: PJExpr;
  s: String;
begin
  current := Elements;
  while Assigned(current) do
  begin
    //quoteString
    if IsString(current^.Right^.Left^.Value) then
      s := current^.Right^.Left^.Value^.vString
    else if TryAsNumber(current^.Right^.Left^.Value) then
      s := AsString(current^.Right^.Left^.Value)
    else //variable
      s := current^.Right^.Left^.Symbol;

    Obj.SetValue(s,EvalExpr(current^.Right^.Right),True);
    current := current^.Left;
  end;
end;

procedure TJEngine.MakeInstance(Obj: TJObject; Members: PJStatement);
//objectにメンバをセットする
var
  current: PJStatement;
  func: IJFunction;
  v: TJValue;
  exp: PJExpr;
begin
  //名前を登録
  Obj.RegistName(Members^.Expr^.Symbol);
  current := Members^.Sub1;
  while Assigned(current) do
  begin
    case current^.SType of
      stFunctionDecl:
      begin
        //関数をセット 変数式が存在しない場合がある
        if Assigned(current^.Expr) then
        begin
          EmptyFunction(func);
          func.Symbol := current^.Expr^.Symbol;
          func.FuncType := ftStatement;
          func.vStatement := current;
          //パラメータセット
          func.Parameter := current^.Sub1;
          //シンボルテーブルを作成
          //親関数から継承する
          func.FunctionTable :=
            FRootTable.GetFunctionTable(current^.Temp,current);
          func.MethodOwner := Obj;
          //メンバに登録する
          Obj.RegistProperty(current^.Expr^.Symbol,BuildFunction(func));
        end
      end;
      //var文と変数宣言
      stVar,stVariableDecl:
      begin
        exp := nil;
        case current^.SType of
          stVar: exp := current^.Sub1^.Expr;
          stVariableDecl: exp := current^.Expr;
        end;

        if Assigned(exp) then
        begin
          if Assigned(exp^.Left) then
            v := EvalExpr(exp^.Left)
          else
            v := BuildNull;

          Obj.RegistProperty(exp^.Symbol,v);
        end;
      end;
    end;
    current := current^.Next;
  end;
end;

function TJEngine.MakeObject(Name: String; Param: TJValueList): TJObject;
{ TODO : ここらへん見直す }
var
  v,con: TJValue;
  objclass: PJObjectClass;
begin
  //ユーザ定義があるか？
  if FCurrentTable.GetValue(Name,v) and
    (IsConstructor(@v) or IsClass(@v)) then
  begin
    case v.vFunction.FuncType of
      //クラス
      ftClass:
      begin
        //super objectが無い場合はobject
        if not Assigned(v.vFunction.vStatement^.Expr^.Left) then
          Result := TJObject.Create(Self,Param)
        else //次へ再帰
          Result := MakeObject(v.vFunction.vStatement^.Expr^.Left^.Symbol,Param);

        //メンバ作成
        MakeInstance(Result,v.vFunction.vStatement);
        //コンストラクタを呼ぶ
        if Result.HasKey(Name) then
        begin
          con := Result.GetValue(Name,False);
          if IsConstructor(@con) then
          begin
            //tempに入れる
            FCurrentTable.AddTemporaryObject(Result);
            CallExpr(con.vFunction,Param,Result);
          end;
        end;

      end;
      //関数
      ftStatement:
      begin
        //objectを作成する
        Result := TJObject.Create(Self);
        //名前を登録
        Result.RegistName(Name);
        //constructor
        Result.RegistProperty('constructor',v);

        //tempに入れる
        FCurrentTable.AddTemporaryObject(Result);
        //constructorを呼ぶ
        CallExpr(v.vFunction,Param,Result);
      end
    else
      //ここは実際には実行されない
      raise EJThrow.Create(E_NAME,'create object constructor error ' + Name);
    end;
  end
  //組込みオブジェクトがあれば返す
  else if FFactory.HasObject(Name) then
  begin
    objclass := FFactory.GetObject(Name);
    Result := objclass^.Create(Self,Param);
    //tempに入れる
    FCurrentTable.AddTemporaryObject(Result);
  end
  else //objectが無いので例外
    raise EJThrow.Create(E_NAME,'create object error ' + Name);
end;

procedure TJEngine.ImportObject(ObjectName: String;
  ObjectClass: TJObjectClass);
begin
  FFactory.ImportObject(ObjectName,ObjectClass);
end;

procedure TJEngine.RegExpOnMatchStart(Sender: TObject);
//マッチ開始 グローバル更新
begin
  FRegExpObject.ClearMatch;
end;

procedure TJEngine.RegExpOnMatchEnd(Sender: TObject);
//マッチ終了 グローバル更新
begin
  FRegExpObject.Assign(Sender as TJRegExpObject);
end;

procedure TJEngine.GlobalObjectOnPrint(Sender: TObject; S: String);
begin
  if Assigned(FOnStdout) then
    FOnStdout(Self,S);
end;

procedure TJEngine.RegistGlobalObjects(ASymbolTable: TJLocalSymbolTable);
//globalオブジェクトを登録する
var
  names: TStringList;
  i: Integer;
  objclass: PJObjectClass;
begin
  //すべてを登録する
  names := FFactory.ObjectNameList;
  for i := 0 to names.Count - 1 do
  begin
    objclass := FFactory.GetObject(names[i]);
    if objclass^.IsMakeGlobalInstance then
    begin
{ TODO : exceptを消したい }
      try
        ASymbolTable.RegistGlobalValue(names[i],BuildObject(objclass^.Create(Self)));
      except
        on EJThrow do
      end;
    end;
  end;
  //重要を上書き
  ASymbolTable.RegistGlobalValue('Global',BuildObject(FGlobalObject));
  ASymbolTable.RegistGlobalValue('RegExp',BuildObject(FRegExpObject));
{$IFNDEF NO_WSH}
  ASymbolTable.RegistGlobalValue('WScript',BuildObject(FWScriptObject));
{$ENDIF}
end;

procedure TJEngine.PrintlnError(S: String);
//stderr
begin
  GlobalObjectOnPrintError(Self,S + CRLF);
  //event
  if Assigned(FOnError) then
    FOnError(Self,FLineNo,S);
end;

procedure TJEngine.GlobalObjectOnPrintError(Sender: TObject; S: String);
begin
  if Assigned(FOnStderr) then
    FOnStderr(Self,S);
end;

function TJEngine.IsRunning: Boolean;
//実行中かどうか
begin
  Result := FIsRunning;
end;

function TJEngine.DoEvents: Boolean;
//待機する
var
{$IFNDEF CONSOLE}
  tid: DWORD;
{$ENDIF}
  abrt: Boolean;
begin
  Result := IsRunning and FAllowEvent;
  //実行中の場合
  if Result then
  begin
    abrt := False;
    //中止チェック
    if Assigned(FOnDoEvents) then
      FOnDoEvents(Self,abrt);
    //中止
    if abrt then
    begin
      Result := False;
      Exit;
    end;
{$IFNDEF CONSOLE}
    //実行スレッドをチェック
    tid := GetCurrentThreadId;
    //メインの場合はメッセージを処理
    if tid = MainThreadId then
    begin
      Result := not Application.Terminated;
      if Result then
        Application.ProcessMessages;
    end;
{$ENDIF}
    //待機
    Sleep(10);
  end;
end;

procedure TJEngine.CallArrayAssign(Parent: TJValue;
  Arguments: PJExpr; Value: TJValue);
//配列 or 関数Callへ代入
// parent(arguments)
// parent[arguments]
var
  s: TJValue;
  param: TJValueList;
begin
  //object場合 配列
  if IsObject(@parent) then
  begin
    //最後の引数を得る
    s := ArgumentsToValue(Arguments);
    //引数が1より大きいときはparamをセットする
    if ArgumentsCount(Arguments) > 1 then
    begin
      //引数セット
      param := ArgumentsToList(Arguments);
      try
        //paramの最後とvは同じ
        parent.vObject.SetValue(AsString(@s),Value,True,param);
      finally
        param.Free;
      end;
    end
    else //一つだけ
      parent.vObject.SetValue(AsString(@s),Value,True);
  end
  //関数の場合
  else if IsFunction(@parent) then
  begin
    //引数セット
    param := ArgumentsToList(Arguments);
    if not Assigned(param) then
      param := TJValueList.Create;
    try
      //値を加える
      param.Add(Value);
      //activexの場合flagをセット
      if parent.vFunction.FuncType = ftActiveX then
        parent.vFunction.vActiveX.Flag := axfPut;
      //関数を呼ぶ
      CallExpr(parent.vFunction,param);
    finally
      param.Free;
    end;
  end
  else
    raise EJThrow.Create(E_CALL,'call function error,need function or object');
end;

function TJEngine.CallArrayExpr(Parent: TJValue; Arguments: PJExpr): TJValue;
//配列 or 関数Call
// parent(arguments)
// parent[arguments]
var
  s: TJValue;
  param: TJValueList;
  obj: TJObject;
begin
  EmptyValue(Result);
  //関数の場合
  if IsFunction(@parent) then
  begin
    //引数セット
    param := ArgumentsToList(Arguments);
    try
      //activexの場合flagをセット
      if parent.vFunction.FuncType = ftActiveX then
        parent.vFunction.vActiveX.Flag := axfGet;
      //関数を呼ぶ thisを継承
      Result := CallExpr(parent.vFunction,param,FCurrentTable.This);
    finally
      param.Free;
    end;
  end
  //objectの場合 配列
  else if IsObject(@parent) or IsString(@parent) then
  begin
    if IsObject(@parent) then
      obj := parent.vObject
    else begin
      obj := MakeObject('String',nil);
      (obj as TJStringObject).text := AsString(@parent);
    end;
    //最後の引数を得る
    s := ArgumentsToValue(Arguments);
    //引数が1より大きいときはparamをセットする
    if ArgumentsCount(Arguments) > 1 then
    begin
      //引数セット
      param := ArgumentsToList(Arguments);
      try
        //paramの最後とvは同じ
        Result := obj.GetValue(AsString(@s),True,param);
      finally
        param.Free;
      end;
    end
    else //一つだけ
      Result := obj.GetValue(AsString(@s),True);
  end
  else
    raise EJThrow.Create(E_CALL,'call function error,need function or object');
end;

procedure TJEngine.GlobalObjectOnRead(Sender: TObject; var S: String; var Success: Boolean;
  Count: Integer; Line: Boolean);
begin
  if Assigned(FOnStdin) then
    FOnStdin(Self,S,Success,Count,Line);
end;

function TJEngine.GetObjectCount: Integer;
begin
  Result := FFactory.ObjectCount;
end;

procedure TJEngine.AfterRun;
//実行後
begin
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

procedure TJEngine.BeforeRun(Main: Boolean);
//実行前
begin
{$IFNDEF NO_WSH}
  //WScriptの引数を解決
  if Main then
    FWScriptObject.Arguments.Parse(FGlobalObject.arguments);
{$ENDIF}
  if Assigned(FOnRun) then
    FOnRun(Self);
end;

procedure TJEngine.MethodAssign(P: PJExpr;
  Value: TJValue);
//メソッド代入
//L ... ParentObject
//R ... Member
//T ... Arguments
var
  l,r,t: PJExpr;
  parent,member: TJValue;
  param: TJValueList;
begin
  if not Assigned(P) then
    Exit;

  l := P^.Left;
  r := P^.Right;
  t := P^.Third;

  //親object
  parent := EvalExpr(l);
  //memberを得る
  member := MemberExpr(parent,r^.Symbol);
  //関数だったらcall
  if IsFunction(@member) then
  begin
    //引数セット
    param := ArgumentsToList(t);
    if not Assigned(param) then
      param := TJValueList.Create;
    try
      //※さらに代入値を最後に加える
      param.Add(Value);
      //activexの場合flagをセット
      if member.vFunction.FuncType = ftActiveX then
        member.vFunction.vActiveX.Flag := axfPut;
      //関数を呼ぶ
      CallExpr(member.vFunction,param,GetThisFromValue(parent));
    finally
      param.Free;
    end;
  end
  else if IsObject(@member) then
  begin
    //Objectの場合は配列
    CallArrayAssign(member,t,Value);
  end
  else
    raise EJThrow.Create(E_CALL,'call function error,need function or object');
end;

function TJEngine.MethodExpr(P: PJExpr): TJValue;
//メソッド式
//postfix_expression DOT[.] variable LSQ[\[] (arguments|null) RSQ[\]]
//postfix_expression DOT[.] variable LP[(] (arguments|null) RP[)]
//L ... ParentObject
//R ... Member
//T ... Arguments
var
  l,r,t: PJExpr;
  parent,member: TJValue;
  param: TJValueList;
begin
  EmptyValue(Result);
  if not Assigned(P) then
    Exit;

  l := P^.Left;
  r := P^.Right;
  t := P^.Third;

  //親object
  parent := EvalExpr(l);
  //memberを得る
  member := MemberExpr(parent,r^.Symbol);
  //関数だったらcall
  if IsFunction(@member) then
  begin
    //引数セット
    param := ArgumentsToList(t);
    try
      //activexの場合flagをセット
      if member.vFunction.FuncType = ftActiveX then
        member.vFunction.vActiveX.Flag := axfGet;
      //関数を呼ぶ
      Result := CallExpr(member.vFunction,param,GetThisFromValue(parent));
    finally
      param.Free;
    end;
  end
  else if IsObject(@member) then
  begin
    //Objectの場合は配列
    Result := CallArrayExpr(member,t);
  end
  else
    raise EJThrow.Create(E_CALL,'call function error,need function or object');
end;

function TJEngine.ArgumentsCount(Arg: PJExpr): Integer;
//argの数を得る
var
  current: PJExpr;
begin
  Result := 0;
  current := Arg;
  while Assigned(current) do
  begin
    Inc(Result);
    current := current^.Left;
  end;
end;

function TJEngine.ArgumentsToList(Arg: PJExpr): TJValueList;
//Listを作って関数パラメータをセット
var
  current: PJExpr;
  v: TJValue;
begin
  if not Assigned(Arg) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJValueList.Create;
  try
    current := Arg;
    while Assigned(current) do
    begin
      v := EvalExpr(current^.Right);
      Result.Insert(0,v);
      current := current^.Left;
    end;
  except
    //例外のときは開放する
    FreeAndNil(Result);
    raise;
  end;
end;

function TJEngine.ArgumentsToValue(Arg: PJExpr): TJValue;
//最後の一つを得る
begin
  //Rightで最後の値を得ることができる
  if Assigned(Arg) then
    Result := EvalExpr(Arg^.Right)
  else
    Result := BuildString('');
end;

function TJEngine.GetThisFromValue(Value: TJValue): TJObject;
//thisを得る
begin
  if IsObject(@Value) then
    Result := Value.vObject
  else if IsString(@Value) then  //文字
  begin
    Result := MakeObject('String',nil);
    (Result as TJStringObject).Text := AsString(@Value);
  end
  else if IsRegExp(@Value) then //正規表現
  begin
    Result := MakeObject('RegExp',nil);
    (Result as TJRegExpObject).SetRegExpValue(Value);
  end
  else if TryAsNumber(@Value) then //数字
  begin
    Result := MakeObject('Number',nil);
    (Result as TJNumberObject).FValue := Value;
  end
  else if IsBool(@Value) then //bool
  begin
    Result := MakeObject('Boolean',nil);
    (Result as TJBooleanObject).FBool := AsBool(@Value);
  end
{$IFNDEF NO_ACTIVEX}
  else if IsDispatch(@Value) then
  begin
    Result := MakeObject('ActiveXObject',nil);
    (Result as TJActiveXObject).disp := AsDispatch(@Value);
  end
{$ENDIF}
  else
    Result := nil;
    //Result := FCurrentTable.This;
end;

function TJEngine.GetVariable(Symbol: String;
  var RetVal: TJValue): Boolean;
//グローバル変数を得る
begin
  Result := FCurrentTable.GetValue(Symbol,RetVal);
end;

function TJEngine.CallEvent(var Event: TJValue; Param: TJValueList;
  This: TJObject): TJValue;
begin
  try
    //関数の場合
    if IsFunction(@Event) then
      Result := CallExpr(Event.vFunction,Param,This)
    //文字列の場合はeval
    else if IsString(@Event) then
      Result := Eval(AsString(@Event),This)
    else
      EmptyValue(Result);
  except
    on E:EJException do
    begin
      //実行中の場合は再生成する
      if not FIsRan then
        raise
      else begin
        //例外、中止のときはイベントループから抜ける
        if E is EJThrow then
        begin
          FAllowEvent := False;
          PrintlnError('Exception: ' +
            E.ExceptName + '(' + IntToStr(FLineNo) + ') => ' + E.Message)
        end
        else if E is EJAbort then
        begin
          FAllowEvent := False;
          PrintlnError('Abort Script(' + IntToStr(FLineNo) + ')')
        end
        else if E is EJSyntaxError then
        begin
          FAllowEvent := False;
          PrintlnError('SyntaxError: ' +
            'Line(' + IntToStr(FLineNo) + ') => ' + E.Message);
        end;
      end;
    end;
  end;
end;

function TJEngine.Run: Integer;
begin
  Result := Run(FParser.Root,nil);
end;

function TJEngine.CallFunction(Symbol: String; Param: TJValueList;
  var RetValue: TJValue): Boolean;
begin
  Result := CallFunction(FParser.Root,Symbol,Param,RetValue);
end;

function TJEngine.Compile(SourceCode: String): Boolean;
//解析木を作る
begin
  Result := False;
  //実行中の場合終わり
  if IsRunning then
    Exit;

  //実行済みフラグをクリア
  FIsRan := False;

  FParser.SourceCode := SourceCode;
  //exe pathを追加
  FParser.LibPath.Add(ExtractFilePath(ParamStr(0)));
  FParser.LibPath.Add(GetCurrentDir);
  try try
    Result := FParser.Parse;
  except
    on E:EJSyntaxError do
    begin
      FLineNo := E.LineNo;
      //event
      PrintlnError('SyntaxError: ' +
          'Line(' + IntToStr(FLineNo) + ') => ' + E.Message);
    end;
    on E:EJThrow do
      PrintlnError('Exception: ' + E.ExceptName + ' => ' + E.Message);
  end;
  finally
    //libpathを削除する
    FParser.LibPath.Delete(FParser.LibPath.Count - 1);
    FParser.LibPath.Delete(FParser.LibPath.Count - 1);
  end;
end;

function TJEngine.CompileFile(AFilename: String; UseBinary: Boolean): Boolean;
//ファイルを指定して実行(lib pathを追加する)

  function GetTempDmc(dmc: String): String;
  var
    path: array[0..MAX_PATH] of Char;
  begin
    GetTempPath(MAX_PATH,path);
    Result := String(path) + ExtractFilename(dmc);
  end;

var
  sl: TStringList;
  dmc,tmpdmc: String;
  ok: Boolean;
begin
  Result := False;
  //実行中の場合終わり
  if IsRunning then
    Exit;

  FFilename := AFilename;
  if not FileExists(AFilename) then
    Exit;

  //実行済みフラグをクリア
  FIsRan := False;

  //コンパイル済みバイナリをロード
  dmc := ChangeFileExt(AFilename,DMS_COMPILED_EXT);
  if UseBinary then
  begin
    //日付を比較して新しければ
    if FileExists(dmc) and (FileAge(dmc) >= FileAge(AFilename)) then
      Result := FParser.Deserialize(dmc);
    //読み込みに失敗したらテンポラリから読む
    if not Result then
    begin
      tmpdmc := GetTempDmc(dmc);
      if FileExists(tmpdmc) and (FileAge(tmpdmc) >= FileAge(AFilename)) then
        Result := FParser.Deserialize(tmpdmc);
    end;
  end;

  if not Result then
  begin
    //lib pathを追加する
    FParser.LibPath.Add(ExtractFilePath(AFilename));
    sl := TStringList.Create;
    try
      sl.LoadFromFile(AFilename);
      Result := Compile(sl.Text);
      //成功ならばシリアライズ
      if Result and UseBinary then
      begin
        ok := FParser.Serialize(dmc);
        //ファイル作成に失敗したらテンポラリに作る
        if not ok then
        begin
          tmpdmc := GetTempDmc(dmc);
          FParser.Serialize(tmpdmc);
        end;
       end;
    finally
      sl.Free;
      //libpathを削除する
      FParser.LibPath.Delete(FParser.LibPath.Count - 1);
    end;
  end;
end;

function TJEngine.GetOnDebugout: TStringEvent;
//イベント
begin
  Result := FParser.Lex.OnDebug
end;

procedure TJEngine.SetOnDebugout(const Value: TStringEvent);
begin
  FParser.Lex.OnDebug := Value;
end;

procedure TJEngine.ClearGlobalObjects;
begin
  FGlobalObject.Clear;
  FRegExpObject.Clear;
{$IFNDEF NO_WSH}
  FWScriptObject.Clear;
{$ENDIF}
end;

function TJEngine.Eval(SourceCode: String; This: TJObject): TJValue;
//eval()
//文字列を関数式に変換して実行する
//function(){return evalコード;}
var
  v: TJValue;
  expr: PJExpr;
  line: Integer;
  //parser: TJParser;
begin
  EmptyValue(Result);
  //行番号保存
  line := FLineNo;
  try
    //parser := TJParser.Create;
    //FParser.Packages.Add(parser);
    //関数式を得る
    expr := FParser.ParseEval(SourceCode);
    if Assigned(expr) then
    begin
      //関数式を評価
      v := EvalExpr(expr);
      if IsFunction(@v) then
      begin
        //currentを親に設定
        v.vFunction.FunctionTable.Parent := CurrentTable.GetNodeTable;
        //実行
        Result := CallExpr(v.vFunction,nil,This);
      end;
    end;
  finally
    FLineNo := line;
  end;
end;

procedure TJEngine.RegExpOnExecInput(Sender: TObject; var Input: String);
begin
  Input := FRegExpObject.input;
end;

function TJEngine.GetScriptFilename: String;
begin
  Result := FFilename;
end;

function TJEngine.FindImportFilename(Filename: String;
  var FindedFilename: String): Boolean;
begin
  Result := FParser.FindImportFilename(Filename,FindedFilename);
end;

end.

