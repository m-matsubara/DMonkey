unit ecma_activex;

//ActiveX Object
//2001/05/03
//by Wolfy

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}


interface

uses
  windows,classes,sysutils,dialogs,syncobjs,gsocketmisc,
  ecma_type,hashtable,ecma_misc,ecma_object,myclasses,
  activex,comobj,AxCtrls;

type
  TConnectionPointCookie = record
    Point: IConnectionPoint;
    Cookie: LongInt;
    Sink: IDispatch;
  end;

  TJActiveXObject = class;

  TJEventSink = class(TInterfacedObject,IDispatch)
  private
    FParent: TJActiveXObject;
    FInfo: ITypeInfo;
  public
    constructor Create(AParent: TJActiveXObject; AInfo: ITypeInfo);
    destructor Destroy; override;

    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  TJActiveXObject = class(TJObject)
  private
    FHash: TIntegerHashTable;
    FDispatch: IDispatch;
    FOwner: IDispatch;
    FSelfDispId: TDispId;
    FPrefix: String;
    FCookies: array of TConnectionPointCookie;

    procedure SetDispatch(const Value: IDispatch);
    procedure SetPrefix(const Value: String);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean; Args: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Args: TJValueList = nil); override;
    function DispIdToString(Id: TDispId): String;
    procedure Clear; override;
    procedure GetPropertyList(List: TStringList); override;
    procedure Connect;
    procedure Disconnect;

    property Prefix: String read FPrefix write SetPrefix;
  published
    property disp: IDispatch read FDispatch write SetDispatch;
  end;

  TJEnumeratorObject = class(TJObject)
  private
    FEnum: IEnumVariant;
    FItem: OleVariant;
    FAtEnd: Boolean;

    function DoAtEnd(Param: TJValueList): TJValue;
    function DoItem(Param: TJValueList): TJValue;
    function DoMoveFirst(Param: TJValueList): TJValue;
    function DoMoveNext(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    class function IsMakeGlobalInstance: Boolean; override;

    function Item: TJValue;
    procedure MoveNext;
    property AtEnd: Boolean read FAtEnd;
  end;


function DispidToStr(Disp: IDispatch; Id: TDispId): String;
//コレクションかどうか
function IsCollection(P: PJValue): Boolean;

procedure RegisterDMS(Engine: TJBaseEngine);


implementation

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('ActiveXObject',TJActiveXObject);
  Engine.ImportObject('Enumerator',TJEnumeratorObject);
end;

function DispidToStr(Disp: IDispatch; Id: TDispId): String;
//dispidから名前を得る
var
  typeinfo: ITypeInfo;
  name: WideString;
begin
  try
    OleCheck(Disp.GetTypeInfo(0,0,typeinfo));
    OleCheck(typeinfo.GetDocumentation(Id, @name, nil, nil, nil));
    Result := name;
  except
  end;
end;

function IsCollection(P: PJValue): Boolean;
//コレクションかどうか
var
  para: TDispParams;
  ret: OleVariant;
  collection: IDispatch;
begin
  Result := False;
  if IsDispatch(P) then
    collection := AsDispatch(P)
  else if IsObject(P) and (P^.vObject is TJActiveXObject) then
    collection := (P^.vObject as TJActiveXObject).disp
  else
    Exit;

  try
    para.rgvarg := nil;
    para.rgdispidNamedArgs := nil;
    para.cArgs := 0;
    para.cNamedArgs := 0;
    VariantInit(ret);

    OLECheck(
      collection.Invoke(
        DISPID_NEWENUM,
        GUID_NULL,
        GetUserDefaultLCID,
        DISPATCH_PROPERTYGET,
        para,@ret,nil,nil));

    Result := True;
  except
  end;
end;


{ TJActiveXObject }

procedure TJActiveXObject.Connect;
//イベント接続
var
  enum: IEnumConnectionPoints;
  p: IConnectionPoint;
  fetched,cookie,num: LongInt;
  sink: IDispatch;
  iid: TIID;
  info: ITypeInfo;
  lib: ITypeLib;
  container: IConnectionPointContainer;
begin
  //イベントクリア
  Disconnect;
  if not Assigned(FDispatch) then
    Exit;

  try
    container := FDispatch as IConnectionPointContainer;
    OleCheck(Container.EnumConnectionPoints(enum));
    if Assigned(enum) then
    begin
{ TODO : 重複しているイベントがあって、どれを選択すればいいのかわからない }
      OleCheck(enum.Next(1,p,@fetched));
      //イベントinterfaceを得る
      OleCheck(p.GetConnectionInterface(iid));
      //イベントinterfaceのITypeInfoを得る
      OleCheck(FDispatch.GetTypeInfo(0,0,info));
      OleCheck(info.GetContainingTypeLib(lib,num));
      OleCheck(lib.GetTypeInfoOfGuid(iid,info));
      //イベント登録
      sink := TJEventSink.Create(Self,info);
      OleCheck(p.Advise(sink,cookie));
      SetLength(FCookies,1);
      FCookies[0].Point := p;
      FCookies[0].Cookie := cookie;
      FCookies[0].Sink := sink;

      {i := 0;
      while enum.Next(1,p,@fetched) = S_OK do
      begin
        try
          //イベントinterfaceを得る
          OleCheck(p.GetConnectionInterface(iid));
          //イベントinterfaceのITypeInfoを得る
          OleCheck(FDispatch.GetTypeInfo(0,0,info));
          OleCheck(info.GetContainingTypeLib(lib,num));
          OleCheck(lib.GetTypeInfoOfGuid(iid,info));
          //イベント登録
          sink := TJEventSink.Create(Self,info);
          OleCheck(p.Advise(sink,cookie));
          SetLength(FCookies,i + 1);
          FCookies[i].Point := p;
          FCookies[i].Cookie := cookie;
          FCookies[i],Sink := sink;
        except
        end;
        //増やす
        Inc(i);
      end; }
    end;
  except
  end;

end;

procedure TJActiveXObject.Disconnect;
//イベント解除
var
  i: Integer;
begin
{ TODO : Sinkを消すタイミングでえらーになる }
  for i := 0 to Length(FCookies) - 1 do
    try
      //sinkのFParentをクリア
      FCookies[i].Sink.GetIDsOfNames(GUID_NULL,nil,-1,-1,nil);
      OleCheck(FCookies[i].Point.Unadvise(FCookies[i].Cookie));
    except
    end;

  FCookies := nil;
end;

procedure TJActiveXObject.Clear;
begin
  inherited;
  //イベントクリア
  Disconnect;
  FHash.Clear;
  FDispatch := nil;
  FOwner := nil;
  FSelfDispId := DISPID_UNKNOWN;
  FPreFix := '';
end;

constructor TJActiveXObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
  s: String;
begin
  inherited;
  RegistName('ActiveXObject');

  FHash := TIntegerHashTable.Create(10);

  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsDispatch(@v) then
      SetDispatch(AsDispatch(@v))
    else begin
      s := AsString(@v);
      try
        SetDispatch(CreateOleObject(s));
      except
        raise EJThrow.Create(E_ACTIVEX,'create error ' + s);
      end;
    end;

    //イベントセット
    if IsParam2(Param) then
    begin
      v := Param[1];
      SetPrefix(AsString(@v));
    end;
  end;
end;

destructor TJActiveXObject.Destroy;
begin
  Clear;
  FreeAndNil(FHash);
  inherited;
end;

function TJActiveXObject.DispIdToString(Id: TDispId): String;
var
  sl: TStringList;
  i: Integer;
begin
  Result := '';
  sl := FHash.KeyList;
  for i := 0 to sl.Count - 1 do
  begin
    if FHash[sl[i]] = Id then
    begin
      Result := sl[i];
      Break;
    end;
  end;
end;

procedure TJActiveXObject.GetPropertyList(List: TStringList);
begin
  EnumDispatchProperties(FDispatch,GUID_NULL,VT_EMPTY,List);
end;

function TJActiveXObject.GetValue(S: String; ArrayStyle: Boolean;
  Args: TJValueList = nil): TJValue;

  function GetDispId(Name: WideString): TDispId;
  begin
    if FHash.HasKey(Name) then
      Result := FHash[Name]
    else begin
      if not Assigned(FDispatch) then
        raise EJThrow.Create(E_ACTIVEX,Name);

      try
        OLECheck(
          FDispatch.GetIDsOfNames(
            GUID_NULL,
            @Name,
            1,
            GetUserDefaultLCID,
            @Result));
      except
        raise EJThrow.Create(E_ACTIVEX,Name);
      end;
      //キャッシュ
      FHash[Name] := Result;
    end;
  end;

var
  di: TDispID;
  param: TDispParams;
  ret,v: OleVariant;
  func: IJFunction;
  arglist: PVariantArgList;
  index,i: Integer;
begin
  EmptyValue(Result);
  //membersにあるならば終り
  if HasKey(S) or HasDefaultProperty(S)  then
  begin
    Result := inherited GetValue(S,ArrayStyle);
    Exit;
  end;
  //配列アクセスではない場合
  if not ArrayStyle then
  begin
    di := GetDispId(S);
    param.rgvarg := nil;
    param.rgdispidNamedArgs := nil;
    param.cArgs := 0;
    param.cNamedArgs := 0;
    VariantInit(ret);
    //property呼び出し引数なし
    try
      OLECheck(FDispatch.Invoke(
        di,
        GUID_NULL,
        GetUserDefaultLCID,
        DISPATCH_PROPERTYGET,
        param,@ret,nil,nil));

      Result := VariantToValue(ret,FEngine);
      //引数を持つプロパティの場合があるのでFDispatchを渡す
      if IsObject(@Result) and (Result.vObject is TJActiveXObject) then
      begin
        (Result.vObject as TJActiveXObject).FOwner := FDispatch;
        (Result.vObject as TJActiveXObject).FSelfDispId := di;
      end;
    except
      EmptyFunction(func);
      func.Symbol := S;
      func.FuncType := ftActiveX;
      func.vActiveX.Dispid := di;
      func.vActiveX.Parent := FDispatch;
      //登録
      Result := BuildFunction(func);
      inherited SetValue(S,Result,ArrayStyle);
    end;
  end
  else begin  //配列アクセスの場合はメソッド呼び出し
    //デフォルトらしい？
    //di := GetDispId('');//Item');
    //di := DISPID_VALUE;

    //2つ以上の引数をとるプロパティがある(Excel.cellsとか)
    if IsParam2(Args) then
    begin
      //2つ以上のときはArgsから
      GetMem(arglist,SizeOf(TVariantArg) * Args.Count);
      //逆順に変換する
      index := 0;
      for i := Args.Count - 1 downto 0 do
      begin
        //tagVariantとOleVariantは同じ
        arglist^[index] := TVariantArg(ValueToVariant(Args[i]));
        Inc(index);
      end;
      param.cArgs := Args.Count;
    end
    else begin
      //1つならSから
      GetMem(arglist,SizeOf(TVariantArg));
      v := S;
      arglist^[0] := TVariantArg(v);
      param.cArgs := 1;
    end;
    param.rgvarg := arglist;
    param.rgdispidNamedArgs := nil;
    param.cNamedArgs := 0;
    VariantInit(ret);

    try try
     if Assigned(FOwner) then
     begin
       OLECheck(
          FOwner.Invoke(
            FSelfDispId,
            GUID_NULL,
            GetUserDefaultLCID,
            DISPATCH_PROPERTYGET or DISPATCH_METHOD,
            param,@ret,nil,nil));

       Result := VariantToValue(ret,FEngine);
     end
     else begin
       OLECheck(
          FDispatch.Invoke(
            DISPID_VALUE,
            GUID_NULL,
            GetUserDefaultLCID,
            DISPATCH_PROPERTYGET or DISPATCH_METHOD,
            param,@ret,nil,nil));

       Result := VariantToValue(ret,FEngine);
     end;
    except
      raise EJThrow.Create(E_ACTIVEX,S);
    end;
    finally
      FreeMem(arglist);
    end;
  end;
end;

procedure TJActiveXObject.SetDispatch(const Value: IDispatch);
//dispatchをセット
begin
  FDispatch := Value;
end;

procedure TJActiveXObject.SetPrefix(const Value: String);
//ここでイベントをセットする
begin
  FPrefix := Value;
  if (FPrefix <> '') then
    Connect;
end;

procedure TJActiveXObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Args: TJValueList = nil);
var
  ws: WideString;
  di,diput: TDispID;
  param: TDispParams;
  v: OleVariant;
  //func: TJFunction;
  arglist: PVariantArgList;
  ary: TJObject;
  i,index: Integer;
begin
  //メンバーに持ってるならば終わり
  if HasDefaultProperty(S)  then
  begin
    inherited;
    Exit;
  end;

  if FHash.HasKey(S) then
    di := FHash[S]
  else begin
    ws := S;
    try
      OLECheck(FDispatch.GetIDsOfNames(
        GUID_NULL,@ws,1,GetUserDefaultLCID,@di));
    except
      raise EJThrow.Create(E_ACTIVEX,S);
    end;
    //キャッシュ
    FHash[S] := di;
  end;

  //初期化
  arglist := nil;
  diput := DISPID_PROPERTYPUT;
  param.rgvarg := nil;
  param.cArgs := 0;
  param.rgdispidNamedArgs := @diput;
  param.cNamedArgs := 1;

  if IsArrayObject(@Value) then
  begin
    //配列型の場合
    ary := Value.vObject;
    if ary.GetCount > 0 then
    begin
      GetMem(arglist,SizeOf(TVariantArg) * ary.GetCount);
      //逆順に変換する
      index := 0;
      for i := ary.GetCount - 1 downto 0 do
      begin
        //tagVariantとOleVariantは同じ
        arglist^[index] := TVariantArg(ValueToVariant(ary.GetItem(i)));
        Inc(Index);
      end;

      param.rgvarg := arglist;
      param.cArgs := ary.GetCount;
    end;
  end
  else begin
    v := ValueToVariant(Value);
    param.rgvarg := @v;
    param.cArgs := 1;
  end;

  //property呼び出し
  try try
    OLECheck(FDispatch.Invoke(
      di,GUID_NULL,GetUserDefaultLCID,
      DISPATCH_PROPERTYPUT,param,nil,nil,nil));
  except
    raise EJThrow.Create(E_ACTIVEX,S);
  end;

  finally
    if Assigned(arglist) then
      FreeMem(arglist);
  end;
end;




{ TJEnumeratorObject }

constructor TJEnumeratorObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
  para: TDispParams;
  ret: OleVariant;
  collection: IDispatch;
begin
  inherited;
  RegistName('Enumerator');
  RegistMethod('atEnd',DoAtEnd);
  RegistMethod('item',DoItem);
  RegistMethod('moveFirst',DoMoveFirst);
  RegistMethod('moveNext',DoMoveNext);

  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsDispatch(@v) then
      collection := AsDispatch(@v)
    else if IsObject(@v) and (v.vObject is TJActiveXObject) then
      collection := (v.vObject as TJActiveXObject).disp
    else
      raise EJThrow.Create(E_ENUMERATOR,'Enumerator.Create Error');

    try
      para.rgvarg := nil;
      para.rgdispidNamedArgs := nil;
      para.cArgs := 0;
      para.cNamedArgs := 0;
      VariantInit(ret);

      OLECheck(
        collection.Invoke(
          DISPID_NEWENUM,
          GUID_NULL,
          GetUserDefaultLCID,
          DISPATCH_PROPERTYGET,
          para,@ret,nil,nil));

      FEnum := IUnknown(ret) as IEnumVariant;
      DoMoveNext(nil);
    except
      raise EJThrow.Create(E_ENUMERATOR,'Enumerator.Create Error');
    end;
  end
  else
    raise EJThrow.Create(E_ENUMERATOR,'Enumerator.Create Error');
end;

function TJEnumeratorObject.DoAtEnd(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FAtEnd);
end;

function TJEnumeratorObject.DoItem(Param: TJValueList): TJValue;
begin
  Result := VariantToValue(FItem,FEngine);
end;

function TJEnumeratorObject.DoMoveFirst(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  FEnum.Reset;
  DoMoveNext(nil);
end;

function TJEnumeratorObject.DoMoveNext(Param: TJValueList): TJValue;
var
  i: Cardinal;
begin
  Result := BuildObject(Self);
  FAtEnd := FEnum.Next(1,FItem,i) <> S_OK;
end;

function TJEnumeratorObject.GetValue(S: String;
  ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
begin
  if S <> '' then
    Result := inherited GetValue(S,ArrayStyle)
  else
    Result := DoItem(nil);
end;

class function TJEnumeratorObject.IsMakeGlobalInstance: Boolean;
begin
  Result := False;
end;

function TJEnumeratorObject.Item: TJValue;
begin
  Result := DoItem(nil);
end;

procedure TJEnumeratorObject.MoveNext;
begin
  DoMoveNext(nil);
end;

{ TJEventSink }

constructor TJEventSink.Create(AParent: TJActiveXObject; AInfo: ITypeInfo);
begin
  inherited Create;
  FParent := AParent;
  FInfo := AInfo;
end;

destructor TJEventSink.Destroy;
begin
  inherited;
end;

function TJEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
//Parentをクリアする
begin
  Result := E_NOTIMPL;
  if IsEqualGUID(IID,GUID_NULL) and (Names = nil) and
    (NameCount = -1) and (LocaleID = -1) and (DispIds = nil) then
  begin
    FParent := nil;
  end;
end;

function TJEventSink.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TJEventSink.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TJEventSink.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
//イベント発生
var
  dispparams: TDispParams;
  name: WideString;
  i: Integer;
  param: TJValueList;
  v: OleVariant;
begin
  Result := S_FALSE;
  if not Assigned(FParent) then
    Exit;

  //名前を得る
  try
    OleCheck(FInfo.GetDocumentation(DispId,@name,nil,nil,nil));
    if name = '' then
      Exit;
  except
    Exit;
  end;

  dispparams := TDispParams(Params);
  param := TJValueList.Create;
  try
    //逆順に入れる
    for i := dispparams.cArgs - 1 downto 0 do
    begin
      try
        v := OleVariant(dispparams.rgvarg^[i]);
      except
        //on VariantError do
        VariantInit(v);
      end;

      param.Add(VariantToValue(v,FParent.FEngine));
    end;

    //イベントを実行する
    FParent.CallEvent(FParent.FPrefix,name,param);
    Result := S_OK;
  finally
    param.Free;
  end;
end;

end.
