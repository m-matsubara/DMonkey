unit hashtbl;

interface

uses
  Windows,Sysutils,Classes;

const
  HASH_10 = 11;
  HASH_20 = 23;
  HASH_30 = 31;
  HASH_50 = 53;
  HASH_100 = 101;

  MAX_DENSITY = 16;  //小さいほど速い
  SLOT_EXTEND = 1.5;
  ENTRY_CACHE = 9;   //奇数にする

type
  THashOperation = (hoNone,hoNew,hoFree,hoInsert,hoDelete);

  PHashEntry = ^THashEntry;
  THashEntry = packed record
    Key: Pointer;
    Value: Pointer;
    Next: PHashEntry;
  end;

  THashEntries = array of PHashEntry;

  //Entryの8バイト境界が問題になるのでキャッシュ
  PHashEntryCache = ^THashEntryCache;  
  THashEntryCache = packed record
    Next: PHashEntryCache;
    Entries: array[0..pred(ENTRY_CACHE)] of THashEntry;
  end;

  TBaseHashTable = class(TObject)
  protected
    FSlotCount: Cardinal;
    FEntryCount: Cardinal;
    FEntries: THashEntries;
    FForEachIndex: Cardinal;

    FFreeEntryList: PHashEntry;
    FEntryCacheList: PHashEntryCache;

    procedure Rehash;
    procedure MakeTable(var Table: THashEntries; Count: Cardinal);
    function MoreEntry: PHashEntry;

    function Compare(Key1,Key2: Pointer): Boolean; virtual;
    function DoHash(Key: Pointer): Cardinal; virtual;
    procedure NewEntry(Key,Value: Pointer; var Entry: THashEntry; Operation: THashOperation); virtual;
    procedure FreeEntry(var Entry: THashEntry; Operation: THashOperation); virtual;

    procedure AddKey(Key: Pointer); virtual;
    procedure DeleteKey(Key: Pointer); virtual;
    procedure ClearKey; virtual;
  public
    constructor Create(DefaultSlotCount: Integer = HASH_10); 
    destructor Destroy; override;

    procedure Clear; virtual;
    function Lookup(Key: Pointer; var Entry: PHashEntry; Operation: THashOperation = hoNone): Boolean;
    procedure AddDirect(Key,Value: Pointer);
    function Insert(Key,Value: Pointer): Boolean;
    function Delete(Key: Pointer): Boolean;

    procedure Foreach(var Entry: PHashEntry);
    function Next(var Entry: PHashEntry): Boolean;
  end;

  TKeyListHashTable = class(TBaseHashTable)
  private
    function GetKeys: String;
    function GetKeyList: TStringList;
  protected
    FKeyList: TStringList;
    FUpdated: Boolean;
    procedure AddKey(Key: Pointer); override;
    procedure DeleteKey(Key: Pointer); override;
    procedure ClearKey; override;
    procedure UpdateKey; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Keys: String read GetKeys;
    property KeyList: TStringList read GetKeyList;
  end;

  TIntegerIntegerHashTable = class(TBaseHashTable)
  public
    function GetValue(Key: Integer; var Value: Integer): Boolean;
    function SetValue(Key: Integer; Value: Integer): Boolean;
    function Remove(Key: Integer): Boolean;
    function HasKey(Key: Integer): Boolean;
  end;

  TStringIntegerHashTable = class(TKeyListHashTable)
  protected
    function Compare(Key1,Key2: Pointer): Boolean; override;
    function DoHash(Key: Pointer): Cardinal; override;
    procedure NewEntry(Key,Value: Pointer; var Entry: THashEntry; Operation: THashOperation); override;
    procedure FreeEntry(var Entry: THashEntry; Operation: THashOperation); override;
    procedure UpdateKey; override;
  public
    function GetValue(Key: String; var Value: Integer): Boolean;
    function SetValue(Key: String; Value: Integer): Boolean;
    function Remove(Key: String): Boolean;
    function HasKey(Key: String): Boolean;
  end;

  TStringStringHashTable = class(TKeyListHashTable)
  protected
    function Compare(Key1,Key2: Pointer): Boolean; override;
    function DoHash(Key: Pointer): Cardinal; override;
    procedure NewEntry(Key,Value: Pointer; var Entry: THashEntry; Operation: THashOperation); override;
    procedure FreeEntry(var Entry: THashEntry; Operation: THashOperation); override;
    procedure UpdateKey; override;
  public
    function GetValue(Key: String; var Value: String): Boolean;
    function SetValue(Key: String; Value: String): Boolean;
    function Remove(Key: String): Boolean;
    function HasKey(Key: String): Boolean;
  end;

  TIntegerStringHashTable = class(TBaseHashTable)
  protected
    function Compare(Key1,Key2: Pointer): Boolean; override;
    function DoHash(Key: Pointer): Cardinal; override;
    procedure NewEntry(Key,Value: Pointer; var Entry: THashEntry; Operation: THashOperation); override;
    procedure FreeEntry(var Entry: THashEntry; Operation: THashOperation); override;
  public
    function GetValue(Key: Integer; var Value: String): Boolean;
    function SetValue(Key: Integer; Value: String): Boolean;
    function Remove(Key: Integer): Boolean;
    function HasKey(Key: Integer): Boolean;
  end;


function HashCodeA(S: String): Cardinal;
function HashCodeB(S: String): Cardinal;


implementation

function HashCodeA(S: String): Cardinal;
//ハッシュの値を得る
//s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]
var
  i,n: Integer;
begin
  Result := 0;
  n := Length(S);
  for i := 1 to n do
    Inc(Result,Byte(S[i]) * 31 xor (n - i));
end;

function HashCodeB(S: String): Cardinal;
//ハッシュの値を得る
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(S[I]);
end;


{ TBaseHashTable }

procedure TBaseHashTable.AddDirect(Key, Value: Pointer);
//無条件登録
var
  hashval,index: Cardinal;
  entry: PHashEntry;
begin
  //要素が多すぎる場合はSlotを増やす
  if (FEntryCount div FSlotCount) > MAX_DENSITY then
    Rehash;

  hashval := DoHash(Key);
  index := hashval mod FSlotCount;

  entry := MoreEntry;
  NewEntry(Key,Value,entry^,hoNew);
  entry^.Next := FEntries[index];
  FEntries[index] := entry;
  Inc(FEntryCount);
  AddKey(Key);
end;

function TBaseHashTable.Compare(Key1, Key2: Pointer): Boolean;
//比較関数
//再定義する
begin
  Result := Key1 = Key2;
end;

constructor TBaseHashTable.Create(DefaultSlotCount: Integer);
//作成する
begin
  inherited Create;
  FSlotCount := DefaultSlotCount;
  MakeTable(FEntries,FSlotCount);
end;

function TBaseHashTable.Delete(Key: Pointer): Boolean;
//要素を削除する
var
  entry: PHashEntry;
begin
  Result := Lookup(Key,entry,hoDelete);
  //開放
  if Result then
  begin
    FreeEntry(entry^,hoFree);
    entry.Next := FFreeEntryList;
    FFreeEntryList := entry;
    
    Dec(FEntryCount);
    DeleteKey(Key);
  end;
end;

destructor TBaseHashTable.Destroy;
//破棄する
begin
  Clear;
  inherited;
end;

function TBaseHashTable.DoHash(Key: Pointer): Cardinal;
//ハッシュ関数
//再定義する
begin
  Result := Cardinal(Key);
end;

procedure TBaseHashTable.Clear;
//すべて開放する

  procedure ClearSlot(var Slot: PHashEntry);
  var
    cur,nxt: PHashEntry;
  begin
    cur := Slot;
    while Assigned(cur) do
    begin
      nxt := cur.Next;
      //開放する
      FreeEntry(cur^,hoFree); 
      cur := nxt;
    end;
    //nilをセット
    Slot := nil;
  end;

  procedure ClearCache;
  var
    cur,nxt: PHashEntryCache;
  begin
    cur := FEntryCacheList;
    while Assigned(cur) do
    begin
      nxt := cur.Next;
      Dispose(cur);
      cur := nxt;
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to Length(FEntries) - 1 do
    ClearSlot(FEntries[i]);

  ClearCache;
  FEntryCount := 0;
  ClearKey;
end;

procedure TBaseHashTable.MakeTable(var Table: THashEntries; Count: Cardinal);
//テーブルを初期化
begin
  SetLength(Table,Count);
{ TODO : 不要かも？ }
  //FillChar(Table[0],SizeOf(PHashEntry) * Count,0);
end;

function TBaseHashTable.Insert(Key, Value: Pointer): Boolean;
//挿入する
//存在していた場合はtrue
var
  entry: PHashEntry;
begin
  Result := Lookup(Key,entry);
  //存在していた
  if Result then
  begin
    //insert
    FreeEntry(entry^,hoInsert);
    NewEntry(Key,Value,entry^,hoInsert);
  end
  else //存在していなかった
    AddDirect(Key,Value);
end;

function TBaseHashTable.Lookup(Key: Pointer;
  var Entry: PHashEntry; Operation: THashOperation): Boolean;
//探す
var
  cur,prev: PHashEntry;
  idx: Cardinal;
begin
  Result := False;
  Entry := nil;

  idx := DoHash(Key) mod FSlotCount;
  cur := FEntries[idx];
  prev := nil;

  while Assigned(cur) do
  begin
    //完全比較
    if Compare(Key,cur^.Key) then
    begin
      Result := True;
      Entry := cur;
      //deleteの時は繋ぎ変え
      if Operation = hoDelete then
      begin
        //topがある時
        if Assigned(prev) then
          prev^.Next := cur^.Next
        else //topがないとき
          FEntries[idx] := cur^.Next;
      end;

      Break;
    end;

    prev := cur;
    cur := cur.Next;
  end;
end;

procedure TBaseHashTable.Rehash;
//slot数を増やす

  procedure Sort(NewEntries: THashEntries; NewCount: Cardinal; Entry: PHashEntry);
  //振り分け
  var
    cur,nxt: PHashEntry;
    index: Integer;
  begin
    cur := Entry;
    while Assigned(cur) do
    begin
      nxt := cur^.Next;

      index := DoHash(cur^.Key) mod NewCount;
      cur^.Next := NewEntries[index];
      NewEntries[index] := cur;

      cur := nxt;
    end;
  end;

var
  newent: THashEntries;
  i: Integer;
begin
  FSlotCount := Trunc(FSlotCount * SLOT_EXTEND);
  MakeTable(newent,FSlotCount);

  for i := 0 to Length(FEntries) - 1 do
    Sort(newent,FSlotCount,FEntries[i]);

  //入れ替え 
  FEntries := newent;
end;

procedure TBaseHashTable.FreeEntry(var Entry: THashEntry;
  Operation: THashOperation);
//entryを開放する
//再定義する
begin
end;

procedure TBaseHashTable.NewEntry(Key,Value: Pointer;
  var Entry: THashEntry; Operation: THashOperation);
//entryを作成する
//再定義する
begin
  Entry.Key := Key;
  Entry.Value := Value;
end;

procedure TBaseHashTable.Foreach(var Entry: PHashEntry);
//foreach初期化する
begin
  FForeachIndex := 0;
  Entry := nil;
end;

function TBaseHashTable.Next(var Entry: PHashEntry): Boolean;
begin
  Result := False;
  while FForeachIndex < FSlotCount do
  begin
    //ない場合は最初
    if not Assigned(Entry) then
      Entry := FEntries[FForeachIndex]
    else //ある場合は次
      Entry := Entry^.Next;
    //最後かどうかチェック
    if Assigned(Entry) then
    begin
      //成功
      Result := True;
      Break;
    end
    else //次へ
      Inc(FForeachIndex);
  end;
end;

procedure TBaseHashTable.AddKey(Key: Pointer);
begin
end;

procedure TBaseHashTable.ClearKey;
begin
end;

procedure TBaseHashTable.DeleteKey(Key: Pointer);
begin
end;

function TBaseHashTable.MoreEntry: PHashEntry;
var
  p: PHashEntryCache;
  i: Integer;
begin
  if Assigned(FFreeEntryList) then
  begin
    Result := FFreeEntryList;
    FFreeEntryList := FFreeEntryList.Next;
  end
  else begin
    New(p);
    p.Next := FEntryCacheList;
    FEntryCacheList := p;

    Result := @p.Entries[0];
    for i := 1 to pred(ENTRY_CACHE) do
    begin
      p.Entries[i].Next := FFreeEntryList;
      FFreeEntryList := @p.Entries[i];
    end;    
  end;
end;

{ TIntegerIntegerHashTable }

function TIntegerIntegerHashTable.GetValue(Key: Integer;
  var Value: Integer): Boolean;
var
  ent: PHashEntry;
begin
  Result := Lookup(Pointer(Key),ent);
  if Result then
    Value := Integer(ent^.Value);
end;

function TIntegerIntegerHashTable.HasKey(Key: Integer): Boolean;
var
  i: Integer;
begin
  Result := GetValue(Key,i);
end;

function TIntegerIntegerHashTable.Remove(Key: Integer): Boolean;
begin
  Result := Delete(Pointer(Key));
end;

function TIntegerIntegerHashTable.SetValue(Key, Value: Integer): Boolean;
begin
  Result := Insert(Pointer(Key),Pointer(Value));
end;

{ TStringIntegerHashTable }

function TStringIntegerHashTable.Compare(Key1, Key2: Pointer): Boolean;
begin
  Result := AnsiSameStr(PString(Key1)^,PString(Key2)^);
end;

function TStringIntegerHashTable.DoHash(Key: Pointer): Cardinal;
begin
  Result := HashCodeB(PString(Key)^);
end;

procedure TStringIntegerHashTable.FreeEntry(var Entry: THashEntry;
  Operation: THashOperation);
begin
  case Operation of
    hoFree:
    begin
      //文字列を開放
      Dispose(PString(Entry.Key));
    end;
    hoInsert:;
  end;
end;

function TStringIntegerHashTable.GetValue(Key: String;
  var Value: Integer): Boolean;
var
  ent: PHashEntry;
begin
  Result := Lookup(@Key,ent);
  if Result then
    Value := Integer(ent^.Value);
end;

function TStringIntegerHashTable.HasKey(Key: String): Boolean;
var
  i: Integer;
begin
  Result := GetValue(Key,i);
end;

procedure TStringIntegerHashTable.NewEntry(Key, Value: Pointer;
  var Entry: THashEntry; Operation: THashOperation);
begin
  case Operation of
    hoNew:
    begin
      //文字列を作成
      New(PString(Entry.Key));
    end;
    hoInsert:;
  end;
  //文字列をコピー
  PString(Entry.Key)^ := PString(Key)^;
  Entry.Value := Value; 
end;

function TStringIntegerHashTable.Remove(Key: String): Boolean;
begin
  Result := Delete(@Key);
end;

function TStringIntegerHashTable.SetValue(Key: String;
  Value: Integer): Boolean;
begin
  Result := Insert(@Key,Pointer(Value));
end;

procedure TStringIntegerHashTable.UpdateKey;
var
  e: PHashEntry;
begin
  if FUpdated then
  begin
    ClearKey;
    Foreach(e);
    while Next(e) do
      FKeyList.Add(PString(e.key)^);
  end;
end;

{ TStringStringHashTable }

function TStringStringHashTable.Compare(Key1, Key2: Pointer): Boolean;
begin
  Result := AnsiSameStr(PString(Key1)^,PString(Key2)^);
end;

function TStringStringHashTable.DoHash(Key: Pointer): Cardinal;
begin
  Result := HashCodeB(PString(Key)^);
end;

procedure TStringStringHashTable.FreeEntry(var Entry: THashEntry;
  Operation: THashOperation);
begin
  case Operation of
    hoFree:
    begin
      //文字列を開放
      Dispose(PString(Entry.Key));
      Dispose(PString(Entry.Value));
    end;
    hoInsert:;
  end;
end;

function TStringStringHashTable.GetValue(Key: String;
  var Value: String): Boolean;
var
  ent: PHashEntry;
begin
  Result := Lookup(@Key,ent);
  if Result then
    Value := PString(ent^.Value)^;
end;

function TStringStringHashTable.HasKey(Key: String): Boolean;
var
  s: String;
begin
  Result := GetValue(Key,s);
end;

procedure TStringStringHashTable.NewEntry(Key, Value: Pointer;
  var Entry: THashEntry; Operation: THashOperation);
begin
  case Operation of
    hoNew:
    begin
      //文字列を作成
      New(PString(Entry.Key));
      New(PString(Entry.Value));
    end;
    hoInsert:;
  end;
  //文字列をコピー
  PString(Entry.Key)^ := PString(Key)^;
  PString(Entry.Value)^ := PString(Value)^;
end;

function TStringStringHashTable.Remove(Key: String): Boolean;
begin
  Result := Delete(@Key);
end;

function TStringStringHashTable.SetValue(Key, Value: String): Boolean;
begin
  Result := Insert(@Key,@Value);
end;

procedure TStringStringHashTable.UpdateKey;
var
  e: PHashEntry;
begin
  if FUpdated then
  begin
    ClearKey;
    Foreach(e);
    while Next(e) do
      FKeyList.Add(PString(e.key)^);
  end;
end;

{ TIntegerStringHashTable }

function TIntegerStringHashTable.Compare(Key1, Key2: Pointer): Boolean;
begin
  Result := Key1 = Key2;
end;

function TIntegerStringHashTable.DoHash(Key: Pointer): Cardinal;
begin
  Result := Cardinal(Key);
end;

procedure TIntegerStringHashTable.FreeEntry(var Entry: THashEntry;
  Operation: THashOperation);
begin
  case Operation of
    hoFree:
    begin
      //文字列を開放
      Dispose(PString(Entry.Value));
    end;
    hoInsert:;
  end;
end;

function TIntegerStringHashTable.GetValue(Key: Integer;
  var Value: String): Boolean;
var
  ent: PHashEntry;
begin
  Result := Lookup(Pointer(Key),ent);
  if Result then
    Value := PString(ent^.Value)^;
end;

function TIntegerStringHashTable.HasKey(Key: Integer): Boolean;
var
  s: String;
begin
  Result := GetValue(Key,s);
end;

procedure TIntegerStringHashTable.NewEntry(Key, Value: Pointer;
  var Entry: THashEntry; Operation: THashOperation);
begin
  case Operation of
    hoNew:
    begin
      //文字列を作成
      New(PString(Entry.Value));
    end;
    hoInsert:;
  end;
  Entry.Key := Key;
  //文字列をコピー
  PString(Entry.Value)^ := PString(Value)^;
end;

function TIntegerStringHashTable.Remove(Key: Integer): Boolean;
begin
  Result := Delete(Pointer(Key));
end;

function TIntegerStringHashTable.SetValue(Key: Integer;
  Value: String): Boolean;
begin
  Result := Insert(Pointer(Key),@Value);
end;

{ TKeyListHashTable }

procedure TKeyListHashTable.AddKey(Key: Pointer);
begin
  FUpdated := True;
end;

procedure TKeyListHashTable.ClearKey;
begin
  FKeyList.Clear;
  FUpdated := False;
end;

constructor TKeyListHashTable.Create;
begin
  inherited Create;
  FKeyList := TStringList.Create;
end;

procedure TKeyListHashTable.DeleteKey(Key: Pointer);
begin
  FUpdated := True;
end;

destructor TKeyListHashTable.Destroy;
begin
  inherited;
  FreeAndNil(FKeyList);
end;

function TKeyListHashTable.GetKeyList: TStringList;
begin
  UpdateKey;
  Result := FKeyList;
end;

function TKeyListHashTable.GetKeys: String;
begin
  UpdateKey;
  Result := TrimRight(FKeyList.Text);
end;

procedure TKeyListHashTable.UpdateKey;
begin
end;



end.
