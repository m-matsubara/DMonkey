unit myclasses;

//クラス
//by Wolfy

{$IFDEF VER130}
  {$DEFINE DELPHI4_5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE DELPHI4_5}
{$ENDIF}

interface

uses
  classes,windows,sysutils,syncobjs
{$IFDEF DELPHI4_5}
  ,consts;
{$ELSE}
  ,rtlconsts;
{$ENDIF}

type
  TListSortCompareObj = function (Item1, Item2: Pointer): Integer of object;
  TSortFuncObj = procedure(SortList: PPointerList; L, R: Integer;
      SCompare: TListSortCompareObj) of object;

  TWorkList = array of Pointer;
  TSortType = (stMerge,stQuick,stInsert);

  TListPlus = class(TList)
  private
    FOnSortFunc: TSortFuncObj;
    FSortType: TSortType;

    procedure QuickSort(SortList: PPointerList; L, R: Integer;
      SCompare: TListSortCompareObj);
    procedure MergeSort(SortList: PPointerList; WorkList: TWorkList;
      Min,Max: Integer; SCompare: TListSortCompareObj);
    procedure InsertSort(SortList: PPointerList; L, R: Integer;
      SCompare: TListSortCompareObj);
  public
    procedure Sort(Compare: TListSortCompareObj);
    property SortType: TSortType read FSortType write FSortType;
    property OnSortFunc: TSortFuncObj read FOnSortFunc write FOnSortFunc;
  end;

  TPointerList = class(TListPlus)
  private
    function ComparePointer(Item1, Item2: Pointer): Integer;
    function Search(Target: Pointer; Min,Max: Integer): Integer;
  public
    class procedure Error(const Msg: string; Data: Integer); override;
    procedure Add(P: Pointer);
    procedure Remove(P: Pointer);
    function IndexOf(P: Pointer): Integer;
  end;

  TBinList = class(TObject)
  protected
    FItems: TList;

    function Get(Index: Integer): Pointer;
    procedure FreeItem(P: Pointer); virtual;
    function GetCount: Integer;
    function Search(P: Pointer; var Middle: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(P: Pointer);
    function Remove(P: Pointer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear; virtual;
    function IndexOf(P: Pointer): Integer;

    property Items[Index: Integer]: Pointer read Get; default;
    property Count: Integer read GetCount;
  end;

  TSafeFileStream = class(TStream)
  private
    FFilename: String;
    FLock: TCriticalSection;
    FMode: Word;
  protected
    FHandle: Integer;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AFilename: String; Mode: Word; RaiseException: Boolean = False): Boolean;
    procedure Close(IfZeroRemove: Boolean = False);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Lock;
    procedure Unlock;
    function SeekWrite(Offset: LongInt; const Buffer; Count: LongInt): LongInt;
    function SeekRead(Offset: LongInt; var Buffer; Count: LongInt): LongInt;
    function IsOpened: Boolean;
    procedure Flush;

    property Filename: String read FFilename;
    property Handle: Integer read FHandle;
    property Mode: Word read FMode;
  end;

  //Mutex
  TMutex = class(TObject)
  private
    FHandle: THandle;
    FName: String;
    FTimeout: DWord;
    FExisted: Boolean;
    
  public
    constructor Create(AName: String);
    destructor Destroy; override;
    function TryLock: Boolean;
    procedure Lock;
    procedure UnLock;
    property Timeout: DWord read FTimeout write FTimeout;
    property Existed: Boolean read FExisted;
  end;
  
  //共有メモリオブジェクト
  EShareMemoryError = class(Exception);
  TShareMemory = class(TObject)
  private
    FHandle: THandle;
    FName: String;
    FList: TList;
    FMutex: TMutex;
    function GetTimeout: DWord;
    procedure SetTimeout(const Value: DWord);
  public
    constructor Create(AName: String; Size: Integer);
    destructor Destroy; override;

    function Open: Pointer;
    procedure Close;
    function TryLock: Boolean;
    procedure Lock;
    procedure UnLock;
    property Timeout: DWord read GetTimeout write SetTimeout;
  end;





implementation

procedure TListPlus.InsertSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompareObj);
//挿入ソート
var
  i,j,res: Integer;
  P: Pointer;
begin
  for i := L + 1 to R do
  begin
    P := SortList^[i];

    j := i - 1;
    while j >= L do
    begin
      res := SCompare(SortList^[j],P);
      if res > 0 then
        SortList^[j + 1] := SortList^[j]
      else
        Break;

      Dec(j);
    end;

    SortList^[j + 1] := P;
  end;
end;

procedure TListPlus.MergeSort(SortList: PPointerList;
  WorkList: TWorkList; Min, Max: Integer; SCompare: TListSortCompareObj);
//マージソートメイン
var
  middle,i1,i2,i3: Integer;
begin
  if (Max - Min) < 1 then
    Exit;

  //２分する
  middle := Max div 2 + Min div 2;
  //再帰
  MergeSort(SortList,WorkList,Min,middle,SCompare);
  MergeSort(SortList,WorkList,middle + 1,Max,SCompare);
  //マージ
  i1 := Min;
  i2 := middle + 1;
  i3 := Min;
  while ((i1 <= middle) and (i2 <= Max)) do
  begin
    if SCompare(SortList^[i1],SortList^[i2]) <= 0 then
    begin
      WorkList[i3] := SortList^[i1];
      Inc(i1);
    end
    else begin
      WorkList[i3] := SortList^[i2];
      Inc(i2);
    end;

    Inc(i3);
  end;

  while i1 <= middle do
  begin
    WorkList[i3] := SortList^[i1];
    Inc(i1);
    Inc(i3);
  end;

  while i2 <= Max do
  begin
    WorkList[i3] := SortList^[i2];
    Inc(i2);
    Inc(i3);
  end;
  //元に戻す
  for i3 := Min to Max do
    SortList^[i3] := WorkList[i3];
end;

procedure TListPlus.QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompareObj);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);

      while SCompare(SortList^[J], P) > 0 do
        Dec(J);

      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;

    until I > J;

    if L < J then
      QuickSort(SortList, L, J, SCompare);

    L := I;

  until I >= R;
end;

procedure TListPlus.Sort(Compare: TListSortCompareObj);
var
  work: TWorkList;
begin
  if (List <> nil) and (Count > 0) then
  begin
    if Assigned(FOnSortFunc) then
      FOnSortFunc(List, 0, Count - 1, Compare)
    else begin
      case FSortType of
        stMerge:
        begin
          //work配列を作成
          SetLength(work,Count);
          try
            //開始
            MergeSort(List,work,0,Count - 1,Compare);
          finally
            work := nil;
          end;
        end;
        stInsert: InsertSort(List,0,Count - 1,Compare);
      else
        QuickSort(List, 0, Count - 1, Compare);
      end;
    end;
  end;
end;


{ TPointerList }

procedure TPointerList.Add(P: Pointer);
begin
  if IndexOf(P) = -1 then
  begin
    inherited Add(P);
    //pointerでソート
    Sort(ComparePointer);
  end;
end;

function TPointerList.ComparePointer(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2);
end;

class procedure TPointerList.Error(const Msg: string; Data: Integer);
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg + ' pointer', [Data]) at ReturnAddr;
end;

function TPointerList.IndexOf(P: Pointer): Integer;
begin
  Result := Search(P,0,Count - 1);
end;

procedure TPointerList.Remove(P: Pointer);
var
  index: Integer;
begin
  index := IndexOf(P);
  if index <> -1 then
    Self.Delete(index);
end;

function TPointerList.Search(Target: Pointer; Min, Max: Integer): Integer;
//補完検索
var
  Middle: Integer;
begin
  while (Min <= Max) do
  begin
    if List[Min] = List[Max] then
    begin
      if List[Min] = Target then
        Result := Min
      else
        Result := -1;

      Exit;
    end;

    Middle := Round(min + ((Integer(Target) - Integer(List[Min])) *
      ((Max - Min) / (Integer(List[Max]) - Integer(List[Min]) ))));

    if (Middle < Min) or (Middle > Max) then
    begin
      Result := -1;
      Exit;
    end;

    if Target = List[Middle] then
    begin
      Result := Middle;
      Exit;
    end
    else if Integer(Target) < Integer(List[Middle]) then
      Max := Middle - 1
    else
      Min := Middle + 1;
  end;

  Result := -1;
end;


{ TSafeFileStream }

procedure TSafeFileStream.Close(IfZeroRemove: Boolean);
//ファイルを閉じる
var
  filesize: Integer;
begin
  if FHandle > 0 then
  begin
    filesize := Size;
    FileClose(FHandle);
    //ファイルサイズが0だった場合に消す
    if (filesize = 0) and IfZeroRemove then
      DeleteFile(FFilename);
  end;
  //-1にしておく
  FHandle := -1;
  FMode := 0;
end;

constructor TSafeFileStream.Create;
//ファイルストリーム
begin
  FLock := TCriticalSection.Create;
  FFilename := '';
  FHandle := -1;
end;

destructor TSafeFileStream.Destroy;
//破棄する
begin
  //閉じる
  Close;
  FLock.Free;  
  inherited;
end;

procedure TSafeFileStream.Flush;
begin
  FlushFileBuffers(FHandle);
end;

function TSafeFileStream.IsOpened: Boolean;
//開いてる？
begin
  Result := FHandle >= 0;
end;

procedure TSafeFileStream.Lock;
//lock
begin
  FLock.Enter;
end;

function TSafeFileStream.Open(const AFilename: String;
  Mode: Word; RaiseException: Boolean): Boolean;
//新しくファイルを開く
begin
  //まず閉じる
  Close;
  //ファイル名
  FFilename := AFilename;
  FMode := Mode;
  //ファイルがなければまず作成する
  if (not FileExists(AFilename)) and ((Mode and fmOpenWrite) <> 0) then
  begin
    FHandle := FileCreate(AFilename);
    //すぐ閉じる
    if FHandle > 0 then
      FileClose(FHandle);
  end;
  //開く
  FHandle := FileOpen(AFileName, Mode);
  Result := (FHandle > 0);
  //例外を起こす？
  if (not Result) and RaiseException then
    raise EFOpenError.CreateResFmt(@SFOpenError, [AFileName]);
end;

function TSafeFileStream.Read(var Buffer; Count: Integer): Longint;
//lockして読み込み
begin
  FLock.Enter;
  try
    //例外を起こして終る
    if FHandle < 0 then
    begin
      Result := 0;
      raise EFOpenError.CreateResFmt(@SFOpenError, [Self.Classname]);
    end;

    Result := FileRead(FHandle, Buffer, Count);
    if Result = -1 then
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSafeFileStream.Seek(Offset: Integer; Origin: Word): Longint;
//lockしてseek
begin
  FLock.Enter;
  try
    //例外を起こして終る
    if FHandle < 0 then
    begin
      Result := 0;
      raise EFOpenError.CreateResFmt(@SFOpenError, [Self.Classname]);
    end;
    
    Result := FileSeek(FHandle, Offset, Origin);
  finally
    FLock.Leave;
  end;
end;

function TSafeFileStream.SeekRead(Offset: Integer; var Buffer;
  Count: Integer): LongInt;
//lockしてseek&read
begin
  FLock.Enter;
  try
    Seek(Offset,soFromBeginning);
    Result := Read(Buffer,Count);
  finally
    FLock.Leave;
  end;
end;

function TSafeFileStream.SeekWrite(Offset: Integer; const Buffer;
  Count: Integer): LongInt;
//lockしてseek&write
begin
  FLock.Enter;
  try
    Seek(Offset,soFromBeginning);
    Result := Write(Buffer,Count);
  finally
    FLock.Leave;
  end;
end;

procedure TSafeFileStream.SetSize(NewSize: Integer);
begin
  Seek(NewSize, soFromBeginning);
  Win32Check(SetEndOfFile(FHandle));
end;

procedure TSafeFileStream.Unlock;
//unlock
begin
  FLock.Leave;
end;

function TSafeFileStream.Write(const Buffer; Count: Integer): Longint;
//lockして書き込み
begin
  FLock.Enter;
  try
    //例外を起こして終る
    if FHandle < 0 then
    begin
      Result := 0;
      raise EFOpenError.CreateResFmt(@SFOpenError, [Self.Classname]);
    end;

    Result := FileWrite(FHandle, Buffer, Count);
    if Result = -1 then
      Result := 0;
  finally
    FLock.Leave;
  end;
end;


{ TMutex }

constructor TMutex.Create(AName: String);
begin
  inherited Create;
  FName := AName;
  FHandle := OpenMutex(MUTEX_ALL_ACCESS,False,PChar(AName));
  if FHandle = 0 then
  begin
    FHandle := CreateMutex(nil,False,PChar(AName));
    FExisted := False;
  end
  else
    FExisted := True;
  //10秒
  FTimeout := 10 * 1000;
end;

destructor TMutex.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

procedure TMutex.Lock;
begin
  WaitForSingleObject(FHandle,INFINITE);
end;

function TMutex.TryLock: Boolean;
begin
  Result := WaitForSingleObject(FHandle,FTimeout) = WAIT_OBJECT_0;
end;

procedure TMutex.UnLock;
begin
  ReleaseMutex(FHandle);
end;



{ TShareMemory }

procedure TShareMemory.Close;
//mapviewを閉じる
begin
  if FList.Count > 0 then
  begin
    UnmapViewOfFile(FList[FList.Count - 1]);
    FList.Delete(FList.Count - 1);
  end;
end;

constructor TShareMemory.Create(AName: String; Size: Integer);
//filemap作成
begin
  inherited Create;

  FHandle :=
      CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,Size,PChar(AName));

  if FHandle = 0 then
    raise EShareMemoryError.Create('fail to file mappiing');

  FName := AName;
  FList := TList.Create;
  FMutex := TMutex.Create(AName + '_MUTEX');
  FMutex.Timeout := 10 * 1000;  
end;

destructor TShareMemory.Destroy;
//終了
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    UnmapViewOfFile(FList[i]);

  FList.Free;
  FMutex.Free;
  CloseHandle(FHandle);
  inherited Destroy;
end;

function TShareMemory.GetTimeout: DWord;
begin
  Result := FMutex.Timeout;
end;

procedure TShareMemory.Lock;
begin
  FMutex.Lock;
end;

function TShareMemory.Open: Pointer;
//mapviewを開く
var
  MapHandle: THandle;
begin
  MapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False,PChar(FName));
  try
    Result := MapViewOfFile(MapHandle,FILE_MAP_ALL_ACCESS,0,0,0);
    FList.Add(Result);
  finally
    CloseHandle(MapHandle);
  end;
end;

procedure TShareMemory.SetTimeout(const Value: DWord);
begin
  FMutex.Timeout := Value;
end;

function TShareMemory.TryLock: Boolean;
begin
  Result := FMutex.TryLock;
end;

procedure TShareMemory.UnLock;
begin
  FMutex.UnLock;
end;



{ TBinList }

procedure TBinList.Add(P: Pointer);
var
  middle: Integer;
begin
  if Search(P,middle) = -1 then
    FItems.Insert(middle,P);
end;

procedure TBinList.Clear;
//すべて削除する
var
  i: Integer;
begin
  for i := FItems.Count - 1 downto 0 do
    Delete(i);

  FItems.Clear;
end;

constructor TBinList.Create;
begin
  inherited;
  FItems := TList.Create;
end;

procedure TBinList.Delete(Index: Integer);
begin
  FreeItem(FItems[Index]);
  FItems.Delete(Index);
end;

destructor TBinList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TBinList.FreeItem(P: Pointer);
begin
  //何もしない
end;

function TBinList.Get(Index: Integer): Pointer;
begin
  Result := FItems[Index];
end;

function TBinList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBinList.IndexOf(P: Pointer): Integer;
var
  middle: Integer;
begin
  Result := Search(P,middle);
end;

function TBinList.Remove(P: Pointer): Integer;
begin
  Result := IndexOf(P);
  if Result > -1 then
    Delete(Result);
end;

function TBinList.Search(P: Pointer; var Middle: Integer): Integer;
//2分検索
var
  min,max,r: Integer;
begin
  min := 0;
  max := FItems.Count - 1;
  middle := 0;
  //バイナリサーチ
  while min <= max do
  begin
    middle := (max + min) div 2;
    //比較
    r := Integer(P) - Integer(FItems[middle]);
    //発見
    if r = 0 then
    begin
      Result := middle;
      Exit;
    end
    else if r < 0 then
    begin
      //keyの方が小さいので 左を探す
      max := middle - 1;
    end
    else begin
      //見つからなかったときのためにmiddleを1つ増やす
      Inc(middle);
      //keyの方が大きので 右を探す
      min := middle;
    end;
  end;
  //みつからなかった
  Result := -1;
end;

end.
