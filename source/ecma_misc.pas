unit ecma_misc;

interface

//その他いろいろ
//2001/04/25~
//by Wolfy
//

{..$DEFINE MB_NO_OWNER}  //オーナーなしのMessageBoxにする

uses
  windows,sysutils,classes,regexpr,shlobj,activex,
{$IFNDEF CONSOLE}
  forms,
{$ENDIF}
  gsocketmisc;

type
  TECMATime = Double;

//日付関係  ECMATime
function SystemTimeToECMATime(SystemTime: TSystemTime): TECMATime;
function ECMATimeToSystemTime(ECMATime: TECMATime): TSystemTime;
function GetLocalTime: TECMATime;
function GetUTCTime: TECMATime;
function DateTimeToECMATime(DateTime: TDateTime): TECMATime;
function ECMATimeToDateTime(ECMATime: TECMATime): TDateTime;
function GetLocalTZA: TECMATime;

//DateTime
function LocalDateTimeToGMT(DateTime: TDateTime): TDateTime;
function GMTToLocalDateTime(DateTime: TDateTime): TDateTime;
function GetTimezone: TDateTime;
function GMTNow: TDateTime;

//日付を解析
function DateParse(S: String): TDateTime;

//整数->文字列
function IntToBitStr(Num: Integer): String;
function IntToOctStr(Num: Integer): String;

//メッセージボックス
function MsgBox(const Text,Caption: String; uType: UINT): Integer; overload;
function MsgBox(Text,Caption: PChar; uType: UINT): Integer; overload;

//アプリケーション名
function GetApplicationTitle: String;

//㍉秒を文字列に
function MSecToStr(MSec: Cardinal): String;
function MSecToStr2(MSec: Cardinal): String;

//bit
const
  BYTE_FLAGS: array[0..7] of Byte =
  (
    1,2,4,8,16,32,64,128
  );

function SetByteFlag(Flags: array of Boolean): Byte;
function GetByteFlag(Flags: Byte; Num: Byte): Boolean;
//位置指定検索
function IndexOf(const Substr, S: string; StartIndex: Integer = 1): Integer;
function LastIndexOf(const Substr, S: string; StartIndex: Integer = 0): Integer;
//マルチバイト文字列
function MBCopy(const S: String; Index, Count: Integer): string;
procedure MBInsert(const Source: string; var S: string; Index: Integer);
procedure MBDelete(var S: string; Index, Count: Integer);
function MBLength(const S: String): Integer;
function MBGetCharAt(const S: String; Index: Integer): String;
procedure MBSetCharAt(const Source: String; var S: String; Index: Integer);
function MBSlice(const S: String; Start,Last: Integer): String;
procedure MBReplace(const Source: String; var S: String; Start,Last: Integer);
function MBIndexOf(const Substr, S: string; StartIndex: Integer = 1): Integer;
function MBLastIndexOf(const Substr, S: string; StartIndex: Integer = 0): Integer;
function MBReverse(const S: string): String;
//文字変換
function Zenkaku(const S: string): string;
function Hankaku(const S: string): string;
function Hiragana(const S: string): string;
function Katakana(const S: string): string;

function ForceDirectories(Dir: string): Boolean;
function DirectoryExists(const Name: string): Boolean;
function SelectFolder(Owner: THandle; const Caption: string; const Root: WideString;
  var Directory: string): Boolean;


implementation

const
  UnixDateDelta = 25569;  //1970/01/01
  MSecsPerDay = SecsPerDay * 1000;

function DateTimeToECMATime(DateTime: TDateTime): TECMATime;
//datetime -> ecma time_t
begin
  Result := Round((DateTime - UnixDateDelta) * MSecsPerDay);
end;

function ECMATimeToDateTime(ECMATime: TECMATime): TDateTime;
//ecma time_t -> datetime
begin
  Result := ECMATime / MSecsPerDay + UnixDateDelta;
end;

function SystemTimeToECMATime(SystemTime: TSystemTime): TECMATime;
//SystemTime -> ECMATime
begin
  Result :=
    DateTimeToECMATime(SystemTimeToDateTime(SystemTime));
end;

function ECMATimeToSystemTime(ECMATime: TECMATime): TSystemTime;
//ECMATime -> SystemTime
begin
  DateTimeToSystemTime(ECMATimeToDateTime(Trunc(ECMATime)),Result);
end;

function GetLocalTime: TECMATime;
var
  time: TSystemTime;
begin
  windows.GetLocalTime(time);
  Result := SystemTimeToECMATime(time);
end;

function GetUTCTime: TECMATime;
var
  time: TSystemTime;
begin
  windows.GetSystemTime(time);
  Result := SystemTimeToECMATime(time);
end;

function GetLocalTZA: TECMATime;
//時差
begin
  Result := GetLocalTime - GetUTCTime;
end;


function GetTimezone: TDateTime;
//時差を取得
var
  snow: TSystemTime;
begin
  GetSystemTime(snow);
  //時差
  Result := SystemTimeTodateTime(snow) - Now;
end;

function LocalDateTimeToGMT(DateTime: TDateTime): TDateTime;
//Local to GMT
begin
  Result := DateTime + GetTimezone;
end;

function GMTToLocalDateTime(DateTime: TDateTime): TDateTime;
//GMT -> LocalTime
begin
  Result := DateTime - GetTimezone;
end;

function GMTNow: TDateTime;
//世界標準時
begin
  Result := LocalDateTimeToGMT(Now);
end;


function DateParse(S: String): TDateTime;
//文字を日付に変換
var
  sl: TStringList;
  y,m,d,ho,mi,se,ms: Word;
begin
  S := Trim(S);
  Result := Now;
  DecodeDate(Result,y,m,d);
  DecodeTime(Result,ho,mi,se,ms);

  try
    Result := StrToDateTime(S);
  except
    //変換失敗 Dec 31, 1999 23:59:59
    on EConvertError do
    begin
      sl := TStringList.Create;
      try
        SplitRegExpr('[,:\s]+',S,sl);
        try
          y := StrToIntDef(sl[2],y);
          //月から1をひく
          m := GetMonth(sl[0]) - 1;
          d := StrToIntDef(sl[1],d);
          ho := StrToIntDef(sl[3],ho);
          mi := StrToIntDef(sl[4],mi);
          se := StrToIntDef(sl[5],se);
          ms := StrToIntDef(sl[6],ms);
        except
          on EStringListError do
        end;

        try
          Result := EncodeDate(y,m,d);
          Result := Result + EncodeTime(ho,mi,se,ms);
        except
          on EConvertError do
        end;

      finally
        sl.Free;
      end;
    end;
  end; //except

end;


function IntToBitStr(Num: Integer): string;
//2進文字列
begin
  Result := '';
  repeat
    Result := Char(Ord('0') + (Num and 1)) + Result;
    Num := Num shr 1;
  until Num = 0;
end;

function IntToOctStr(Num: Integer): string;
//8進文字列
begin
  Result := '';
  repeat
    Result := Char(Ord('0') + (Num and 7)) + Result;
    Num := Num shr 3;
  until Num = 0;
end;


function MsgBox(const Text,Caption: String; uType: UINT): Integer;
//メッセージボックス
begin
  Result := MsgBox(PChar(Text), PChar(Caption), uType);
end;

function MsgBox(Text,Caption: PChar; uType: UINT): Integer;
{$IFNDEF MB_NO_OWNER}
var
  old,app: HWND;
{$ENDIF}
begin
{$IFDEF MB_NO_OWNER}
  Result := MessageBox(0, Text, Caption, uType);
{$ELSE}
  old := GetActiveWindow;
  {$IFNDEF CONSOLE}
    app := Application.Handle;
  {$ELSE}
    app := 0;
  {$ENDIF}

  Result := MessageBox(app,Text,Caption,uType);
  if old <> 0 then
    SetForegroundWindow(old);
{$ENDIF}
end;


//アプリケーション名
function GetApplicationTitle: String;
begin
{$IFNDEF CONSOLE}
  Result := Application.Title;
{$ELSE}
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  Result := AnsiUpperCase(Copy(Result,1,1)) + AnsiLowerCase(Copy(Result,2,MaxInt));
{$ENDIF}
end;


function MSecToStr(MSec: Cardinal): String;
//ミリ秒を時間に変換してstringに
var
 h,n,s,tmp: Cardinal;
begin
  if MSec > 0 then
  begin
    tmp := Msec div 1000;

    h := tmp div 3600;
    tmp := tmp mod 3600;
    n := tmp div 60;
    s := tmp mod 60;
  end
  else begin
    h := 0;
    n := 0;
    s := 0;
  end;

  Result := Format('%u:%.2u:%.2u',[h,n,s]);
end;

function MSecToStr2(MSec: Cardinal): String;
//ミリ秒を時間に変換してstringに
var
 h,n,s,tmp: Cardinal;
begin
  if MSec > 0 then
  begin
    tmp := MSec div 1000;
    MSec := MSec mod 1000;

    h := tmp div 3600;
    tmp := tmp mod 3600;
    n := tmp div 60;
    s := tmp mod 60;
  end
  else begin
    h := 0;
    n := 0;
    s := 0;
    MSec := 0;
  end;

  Result := Format('%u:%.2u:%.2u:%.3u',[h,n,s,MSec]);
end;


function SetByteFlag(Flags: array of Boolean): Byte;
//bit flagをセット
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Flags) do
    if Flags[i] then
      Result := Result or BYTE_FLAGS[i];
end;

function GetByteFlag(Flags: Byte; Num: Byte): Boolean;
//bit flagを得る 0～7
begin
  Result := ((Flags shr Num) and 1) = 1;
end;


function IndexOf(const Substr, S: string; StartIndex: Integer): Integer;
//開始位置を指定できるPos
var
  tmp: string;
begin
  if StartIndex > 1 then
  begin
    tmp := Copy(S,StartIndex,MaxInt);
    Result := Pos(Substr,tmp);
    if Result > 0 then
      Inc(Result,StartIndex - 1);
  end
  else
    Result := Pos(Substr,S);
end;

function LastIndexOf(const Substr, S: string; StartIndex: Integer): Integer;
//後ろから探す
var
  len,idx,i: Integer;
  p1,p2: PChar;
begin
  Result := 0;

  len := Length(Substr);
  //開始位置のデフォルト
  idx := Length(S) - len;
  //StartIndexが0以下のときは右端から
  if (StartIndex > 0) and (StartIndex - 1 < idx) then
    idx := StartIndex - 1;

  p1 := PChar(S);
  p2 := PChar(Substr);
  //逆からバイナリ比較
  for i := idx downto 0 do
  begin
    if CompareMem(p1 + i,p2,len) then
    begin
      Result := i + 1;
      Break;
    end;
  end;
end;


function MBCopy(const S: String; Index, Count: Integer): string;
//MBコピー
var
  start,last: Integer;
begin
  Result := '';
  if Count > 0 then
  begin
    if Index < 1 then
      Index := 1;
    start := CharToByteIndex(S,Index);
    if start > 0 then
    begin
      last := CharToByteIndex(S,Index + Count);
      if last = 0 then
        last := MaxInt;
      Result := Copy(S,start,last - start);
    end;
  end;
end;

procedure MBInsert(const Source: string; var S: string; Index: Integer);
//MB挿入
var
  i: Integer;
begin
  if Index < 1 then
    Index := 1;
  i := CharToByteIndex(S,Index);
  if i > 0 then
    Insert(Source,S,i);
end;

procedure MBDelete(var S: string; Index, Count: Integer);
//MB削除
var
  start,last: Integer;
begin
  if Count > 0 then
  begin
    start := CharToByteIndex(S,Index);
    last := CharToByteIndex(S,Index + Count);
    if last = 0 then
      last := MaxInt;
    Delete(S,start,last - start);
  end;
end;

function MBLength(const S: String): Integer;
//MB文字数
begin
  Result := ByteToCharLen(S,MaxInt);
end;

function MBGetCharAt(const S: String; Index: Integer): String;
//MB s[]
begin
  //0以下のときは空文字
  if Index > 0 then
    Result := MBCopy(S,Index,1)
  else
    Result := '';
end;

procedure MBSetCharAt(const Source: String; var S: String; Index: Integer);
//MB s[] :=
begin
  MBDelete(S,Index,1);
  MBInsert(Source,S,Index);
end;

function MBSlice(const S: String; Start,Last: Integer): String;
//Start - (Last - 1)文字までをコピー
var
  len: Integer;
begin
  //長さ調節
  if (Start < 1) or (Last < 1) then
  begin
    len := MBLength(S);
    if Start < 1 then
    begin
      Start := len + Start;
      if Start < 1 then
        Start := 1;
    end;

    if Last < 1 then
      Last := len + Last;
  end;

  Result := MBCopy(S,Start,Last - Start);
end;

procedure MBReplace(const Source: String; var S: String; Start,Last: Integer);
//Start - (Last - 1)文字までを置換
var
  len: Integer;
begin
  //長さ調節
  if (Start < 1) or (Last < 1) then
  begin
    len := MBLength(S);
    if Start < 1 then
      Start := len + Start;

    if Last < 1 then
      Last := len + Last;
  end;

  MBDelete(S,Start,Last - Start);
  MBInsert(Source,S,Start);
end;

function MBIndexOf(const Substr, S: string; StartIndex: Integer): Integer;
var
  v: String;
begin
  if StartIndex > 1 then
    v := MBCopy(S,StartIndex,MaxInt)
  else
    v := S;

  Result := ByteToCharIndex(v,AnsiPos(SubStr,v));
  if Result > 0 then
    Result := Result + StartIndex - 1;
end;

function MBLastIndexOf(const Substr, S: string; StartIndex: Integer): Integer;
var
  v: String;
  len: Integer;
begin
  len := MBLength(Substr);
  if StartIndex > 0 then
    v := MBReverse(MBCopy(S,1,StartIndex + len - 1))
  else
    v := MBReverse(S);

  Result := ByteToCharIndex(v,AnsiPos(MBReverse(Substr),v));
  if Result > 0 then
    Result := MBLength(v) - Result - len + 2;
end;

function MBReverse(const S: string): String;
var
  i,j,len: Integer;
  c: Char;
begin
  len := Length(S);
  SetLength(Result,len);
  i := 1;
  j := len;
  while i < len do
  begin
    c := S[i];
    if c in LeadBytes then
    begin
      Inc(i);
      Result[j] := S[i];
      Dec(j);
    end;
    Result[j] := c;
    Inc(i);
    Dec(j);
  end;
  if i = len then
    Result[j] := S[i];
end;


function MapString(const Source: string; Flags: DWORD): string;
//別の文字列にマップ
var
  dest: PChar;
  len: Integer;
begin
  Result := '';
  len := Length(Source) * 2 + 1;
  GetMem(dest,len);
  FillChar(dest^,len,#0);//要らない？
  try
    LCMapString(GetUserDefaultLCID,Flags,PChar(Source),Length(Source) + 1,dest,len);
    Result := dest;
  finally
    FreeMem(dest);
  end;
end;

function Zenkaku(const S: string): string;
//半角を全角に変換
begin
  Result := MapString(S,LCMAP_FULLWIDTH);
end;

function Hankaku(const S: string): string;
//全角を半角に変換
begin
  Result := MapString(S,LCMAP_HALFWIDTH);
end;

function Hiragana(const S: string): string;
//全角カタカナを全角ひらがなへ
begin
  Result := MapString(S,LCMAP_HIRAGANA);
end;

function Katakana(const S: string): string;
//全角ひらがなを全角カタカナへ
begin
  Result := MapString(S,LCMAP_KATAKANA);
end;


function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    raise Exception.Create('Cannot Create Dir');
  Dir := ExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  result := 0;
end;

function SelectFolder(Owner: THandle; const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  //WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';

  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Owner, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := Owner;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
      //WindowList := DisableTaskWindows(0);
      OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        SetErrorMode(OldErrorMode);
        //EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;





end.
