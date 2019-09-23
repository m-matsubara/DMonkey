unit ecma_extobject;

//拡張組込みobject
//2001/4/26 ~
//by Wolfy

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  windows,classes,sysutils,dialogs,syncobjs,
{$IFNDEF CONSOLE}
  forms,
{$ENDIF}
  filectrl,
  gsocketmisc,
  ecma_type,hashtable,ecma_re,
  jconvert,ecma_misc,ecma_object,myclasses,
  IniFiles,crclib,
  clipbrd,
  shellapi,
  registry,
  misc,Math;

type
  TJStringsObject = class;

  //ファイルobject
  TJFileObject = class(TJObject)
  private
    FFile: TSafeFileStream;
    FFileName: String;

    function DoRead(Param: TJValueList): TJValue;
    function DoOpen(Param: TJValueList): TJValue;
    function DoClose(Param: TJValueList): TJValue;
    function DoIsOpened(Param: TJValueList): TJValue;
    function DoWrite(Param: TJValueList): TJValue;
    function DoWriteln(Param: TJValueList): TJValue;
    function DoFlush(Param: TJValueList): TJValue;
    function DoReadln(Param: TJValueList): TJValue;
    function DoEof(Param: TJValueList): TJValue;
    function DoExists(Param: TJValueList): TJValue;
    function DoRemove(Param: TJValueList): TJValue;
    function DoRenameTo(Param: TJValueList): TJValue;
    function DoCopyTo(Param: TJValueList): TJValue;
    function DoCanRead(Param: TJValueList): TJValue;
    function DoCanWrite(Param: TJValueList): TJValue;
    function DoSeek(Param: TJValueList): TJValue;
    function DoExtractName(Param: TJValueList): TJValue;
    function DoExtractPath(Param: TJValueList): TJValue;
    function DoExtractDir(Param: TJValueList): TJValue;
    function DoExtractExt(Param: TJValueList): TJValue;
    function DoExtractDrive(Param: TJValueList): TJValue;
    function DoChangeExt(Param: TJValueList): TJValue;
    function DoExpandUNCFilename(Param: TJValueList): TJValue;
    function DoExtractShortPathName(Param: TJValueList): TJValue;

    function GetLength: Integer;
    function GetLastModified: TJDateObject;
    function GetTargetFilename(Param: TJValueList): String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property length: Integer read GetLength;
    property lastModified : TJDateObject read GetLastModified;
    property filename: String read FFilename write FFilename;
    property path: String read FFilename write FFilename;
  end;
  //ディレクトリ
  TJDirectoryObject = class(TJObject)
  private
    FDirname: String;
    FSearchRec: TSearchRec;
    FBeginFind: Boolean;

    function DoMake(Param: TJValueList): TJValue;
    function DoRemove(Param: TJValueList): TJValue;
    function DoRenameTo(Param: TJValueList): TJValue;
    function DoFindFirst(Param: TJValueList): TJValue;
    function DoFindFirstFile(Param: TJValueList): TJValue;
    function DoFindNext(Param: TJValueList): TJValue;
    function DoFindNextFile(Param: TJValueList): TJValue;
    function DoFindNextDir(Param: TJValueList): TJValue;
    function DoFindFirstDir(Param: TJValueList): TJValue;
    function DoFindClose(Param: TJValueList): TJValue;
    function DoExists(Param: TJValueList): TJValue;
    function DoChangeTo(Param: TJValueList): TJValue;
    function DoExcludePathDelimiter(Param: TJValueList): TJValue;
    function DoIncludePathDelimiter(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoFindFiles(Param: TJValueList): TJValue;
    function DoExpandUNCFilename(Param: TJValueList): TJValue;

    procedure SetDirname(const Value: String);
    function GetDirectories: TJStringsObject;
    function GetFiles: TJStringsObject;
    function GetWildCard(Param: TJValueList): String;
    function GetTargetDirname(Param: TJValueList): String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property dirname: String read FDirname write SetDirname;
    property path: String read FDirname write SetDirname;
    property files: TJStringsObject read GetFiles;
    property directories: TJStringsObject read GetDirectories;
  end;
  //文字列リスト

  TJBaseStringsObject = class(TJObject)
  private
    FStrings: TStrings;

    function DoAdd(Param: TJValueList): TJValue;
    function DoInsert(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoIndxOf(Param: TJValueList): TJValue;
    function DoSaveToFile(Param: TJValueList): TJValue;
    function DoLoadFromFile(Param: TJValueList): TJValue;
    function DoAssign(Param: TJValueList): TJValue;
    function DoAddStrings(Param: TJValueList): TJValue;
    function GetCommaText: String;
    function GetText: String;
    procedure SetCommaText(const Value: String);
    procedure SetText(const Value: String);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    function ToString(Value: PJValue = nil): String; override;

    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;

    property Strings: TStrings read FStrings write FStrings;
  published
    property text: String read GetText write SetText;
    property commaText: String read GetCommaText write SetCommaText;
    property length: Integer read GetCount;
    property count: Integer read GetCount;
  end;

  TJStringsObject = class(TJBaseStringsObject)
  private
    function DoSort(Param: TJValueList): TJValue;

    function GetStrings: TStringList;
    function GetSorted: Boolean;
    function GetDuplicates: Boolean;
    function GetCaseSensitive: Boolean;
    procedure SetSorted(const Value: Boolean);
    procedure SetDuplicates(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;

    property Strings: TStringList read GetStrings;
  published
    property sorted: Boolean read GetSorted write SetSorted;
    property duplicates: Boolean read GetDuplicates write SetDuplicates;
    property caseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;


  //win32 APIなど
  TJWin32Object = class(TJObject)
  private
    function DoSleep(Param: TJValueList): TJValue;
    function DoWinExec(Param: TJValueList): TJValue;
    function DoShellExecute(Param: TJValueList): TJValue;
    function DoGetTickCount(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  end;
  //iniファイル
  TJIniObject = class(TJObject)
  private
    FIni: TMemIniFile;
    function DoDeleteKey(Param: TJValueList): TJValue;
    function DoEraseSection(Param: TJValueList): TJValue;
    function DoReadSection(Param: TJValueList): TJValue;
    function DoReadSections(Param: TJValueList): TJValue;
    function DoSectionExists(Param: TJValueList): TJValue;
    function DoWrite(Param: TJValueList): TJValue;
    function DoRead(Param: TJValueList): TJValue;
    function DoUpdate(Param: TJValueList): TJValue;
    function GetFilename: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    class function IsMakeGlobalInstance: Boolean; override;
  published
    property filename: String read GetFilename;
  end;
  //CRC
  TJCRCObject = class(TJObject)
  private
    FCRC16: Word;
    FCRC32: DWord;
    function DoCalc(Param: TJValueList): TJValue;
    function DoCalcFile(Param: TJValueList): TJValue;
    function GetCRC16: Integer;
    function GetCRC32: Integer;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property CRC16: Integer read GetCRC16;
    property CRC32: Integer read GetCRC32;
  end;
  //base64
  TJBase64Object = class(TJObject)
  private
    function DoEncode(Param: TJValueList): TJValue;
    function DoDecode(Param: TJValueList): TJValue;
    function DoEncodeHeader(Param: TJValueList): TJValue;
    function DoDecodeHeader(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  end;
  //ダイアログ
  TJDialogObject = class(TJObject)
  private
    FFilters: TJStringsObject;
    function GetFilter: string;

    function DoOpenFile(Param: TJValueList): TJValue;
    function DoOpenFiles(Param: TJValueList): TJValue;
    function DoSaveFile(Param: TJValueList): TJValue;
    function DoOpenFolder(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property filters: TJStringsObject read FFilters;
  end;

  TJMutexObject = class(TJObject)
  private
    FMutex: TMutex;
    function DoLock(Param: TJValueList): TJValue;
    function DoUnLock(Param: TJValueList): TJValue;
    function DoTryLock(Param: TJValueList): TJValue;
    function DoExists(Param: TJValueList): TJValue;
    function GetTimeout: Integer;
    procedure SetTimeout(const Value: Integer);
    function GetExisted: Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property timeout: Integer read GetTimeout write SetTimeout;
    property existed: Boolean read GetExisted;
  end;

  TJPoint = class(TJObject)
  private
    function GetX: Integer;
    function GetY: Integer;
    function GetPt: String;
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
    procedure SetPt(const Value: String);
  public
    __Point: TPoint;
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property x: Integer read GetX write SetX;
    property y: Integer read GetY write SetY;
    property point: String read GetPt write SetPt;
  end;

  //keyboard
  TJKeyboard = class(TJObject)
  protected
    function GetAlt: Boolean;
    function GetCtrl: Boolean;
    function GetShift: Boolean;
    function DoIsDown(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property shift: Boolean read GetShift;
    property ctrl: Boolean read GetCtrl;
    property alt: Boolean read GetAlt;
  end;

  //mouse
  TJMouse = class(TJObject)
  protected
    function GetLButton: Boolean;
    function GetMButton: Boolean;
    function GetRButton: Boolean;
    function GetX: Integer;
    function GetY: Integer;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property lbutton: Boolean read GetLButton;
    property mbutton: Boolean read GetMButton;
    property rbutton: Boolean read GetRButton;
    property x: Integer read GetX;
    property y: Integer read GetY;
  end;

  TJRect = class(TJObject)
  private
    FTopLeft: TJPoint;
    FBottomRight: TJPoint;
    function GetBottom: Integer;
    function GetBottomRight: TJObject;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    function GetTopLeft: TJObject;
    procedure SetBottom(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    function Get__Rect: TRect;
    procedure Set__Rect(const Value: TRect);
    function GetRect: String;
    procedure SetRect(const Value: String);
    procedure SetBottomRight(const Value: TJObject);
    procedure SetTopLeft(const Value: TJObject);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function ToString(Value: PJValue = nil): String; override;
    property __Rect: TRect read Get__Rect write Set__Rect;
  published
    property left: Integer read GetLeft write SetLeft;
    property top: Integer read GetTop write SetTop;
    property right: Integer read GetRight write SetRight;
    property bottom: Integer read GetBottom write SetBottom;
    property topLeft: TJObject read GetTopLeft write SetTopLeft;
    property bottomRight: TJObject read GetBottomRight write SetBottomRight;
    property rect: String read GetRect write SetRect;
  end;


  //clipboard
  TJClipboard = class(TJObject)
  private
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    function DoHasText(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property asText: String read GetAsText write SetAsText;
  end;

  //iniファイル
  TJRegIniObject = class(TJObject)
  private
    FReg: TRegIniFile;

    function DoDeleteKey(Param: TJValueList): TJValue;
    function DoEraseSection(Param: TJValueList): TJValue;
    function DoReadSection(Param: TJValueList): TJValue;
    function DoReadSections(Param: TJValueList): TJValue;
    function DoSectionExists(Param: TJValueList): TJValue;
    function DoWrite(Param: TJValueList): TJValue;
    function DoRead(Param: TJValueList): TJValue;
    function GetKey: String;
    function GetRootKey: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    class function IsMakeGlobalInstance: Boolean; override;
  published
    property rootKey: String read GetRootKey;
    property key: String read GetKey;
  end;

  TJStringBufferObject = class(TJObject)
  private
    FBuffer: String;

    function DoAppend(Param: TJValueList): TJValue;
    function DoCharCodeAt(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoIndexOf(Param: TJValueList): TJValue;
    function DoInsert(Param: TJValueList): TJValue;
    function DoLastIndexOf(Param: TJValueList): TJValue;
    function DoReplace(Param: TJValueList): TJValue;
    function DoSlice(Param: TJValueList): TJValue;
    function DoSubStr(Param: TJValueList): TJValue;
    function DoSubString(Param: TJValueList): TJValue;
    function DoFill(Param: TJValueList): TJValue;

    function GetStringIndex(S: String): Integer;
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    function ToString(Value: PJValue = nil): String; override;

    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property length: Integer read GetLength write SetLength;
    property text: String read FBuffer write FBuffer;
  end;


function IsStringBufferObject(P: PJValue): Boolean;

procedure RegisterDMS(Engine: TJBaseEngine);


implementation

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('File',TJFileObject);
  Engine.ImportObject('Directory',TJDirectoryObject);
  Engine.ImportObject('Strings',TJStringsObject);
  Engine.ImportObject('Win32',TJWin32Object);
  Engine.ImportObject('Ini',TJIniObject);
  Engine.ImportObject('CRC',TJCRCObject);
  Engine.ImportObject('Base64',TJBase64Object);
  Engine.ImportObject('Dialog',TJDialogObject);
  Engine.ImportObject('Mutex',TJMutexObject);
  Engine.ImportObject('Keyboard',TJKeyboard);
  Engine.ImportObject('Mouse',TJMouse);
  Engine.ImportObject('Clipboard',TJClipboard);
  Engine.ImportObject('RegIni',TJRegIniObject);
  Engine.ImportObject('StringBuffer',TJStringBufferObject);
end;

function IsStringBufferObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJStringBufferObject);
end;


{ TJFileObject }

constructor TJFileObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('File');
  FFile := TSafeFileStream.Create;
  //ファイル名設定
  if IsParam1(Param) then
  begin
    v := Param[0];
    FFilename := AsString(@v);
  end;

  RegistMethod('read',DoRead);
  RegistMethod('open',DoOpen);
  RegistMethod('close',DoClose);
  RegistMethod('isOpened',DoIsOpened);
  RegistMethod('write',DoWrite);
  RegistMethod('writeln',DoWriteln);
  RegistMethod('flush',DoFlush);
  RegistMethod('readln',DoReadln);
  RegistMethod('eof',DoEof);
  RegistMethod('exists',DoExists);
  RegistMethod('remove',DoRemove);
  RegistMethod('renameTo',DoRenameTo);
  RegistMethod('copyTo',DoCopyTo);
  RegistMethod('canRead',DoCanRead);
  RegistMethod('canWrite',DoCanWrite);
  RegistMethod('toString',DoToString);
  RegistMethod('seek',DoSeek);
  RegistMethod('extractName',DoExtractName);
  RegistMethod('extractPath',DoExtractPath);
  RegistMethod('extractDir',DoExtractDir);
  RegistMethod('extractDrive',DoExtractDrive);
  RegistMethod('extractExt',DoExtractExt);
  RegistMethod('changeExt',DoChangeExt);
  RegistMethod('expandUNCFilename',DoExpandUNCFilename);
  RegistMethod('extractShortPathName',DoExtractShortPathName);
end;

destructor TJFileObject.Destroy;
begin
  FFile.Close;
  FreeAndNil(FFile);
  inherited;
end;

function TJFileObject.DoCanRead(Param: TJValueList): TJValue;
begin
  Result := BuildBool((FFile.Mode and fmOpenRead) <> 0);
end;

function TJFileObject.DoCanWrite(Param: TJValueList): TJValue;
begin
  Result := BuildBool((FFile.Mode and fmOpenWrite) <> 0);
end;

function TJFileObject.DoClose(Param: TJValueList): TJValue;
//ファイルを閉じる
begin
  Result := BuildBool(FFile.IsOpened);
  FFile.Close;
end;

function TJFileObject.DoCopyTo(Param: TJValueList): TJValue;
//コピー
var
  v: TJValue;
  s: String;
begin
  FFile.Close;
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    if CopyFile(PChar(FFilename),PChar(s),False) then
    begin
      Result := BuildBool(True);
    end
    else
      raise EJThrow.Create(E_FILE,'copy error: ' + FFilename);
  end
  else
    raise EJThrow.Create(E_FILE,'copy error: ' + FFilename);
end;

function TJFileObject.DoEof(Param: TJValueList): TJValue;
begin
  if FFile.IsOpened then
    Result := BuildBool(FFile.Position >= FFile.Size)
  else
    raise EJThrow.Create(E_FILE,'eof error: ' + FFilename);
end;

function TJFileObject.DoFlush(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FFile.IsOpened);
  FFile.Flush;
end;

function TJFileObject.DoIsOpened(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FFile.IsOpened);
end;

function TJFileObject.DoOpen(Param: TJValueList): TJValue;
//ファイルを開く
var
  v: TJValue;
  mode: Integer;
  s: String;
begin
  Result := BuildBool(True);
  mode := 0;
  if IsParam1(Param) then
  begin
    //ファイルモード
    v := Param[0];
    s := LowerCase(AsString(@v));
    if Pos('r',s) > 0 then
      mode := fmOpenRead;

    if Pos('w',s) > 0 then
      mode := mode or fmOpenWrite;

    if Pos('d',s) > 0 then
      mode := mode or fmShareDenyWrite;
  end
  else
    mode := fmOpenRead or fmShareDenyWrite;
  //ファイルを開く
  try
    FFile.Open(FFilename,mode,True);
  except
    on EFOpenError do
      raise EJThrow.Create(E_FILE,'can not open: ' + FFilename);
  end;
end;

function TJFileObject.DoRead(Param: TJValueList): TJValue;
const
  BUFSIZE = 65535;
var
  s,buf: String;
  res,size: Integer;
  v: TJValue;
begin
  Result := BuildNull;
  s := '';
  if FFile.IsOpened then
  begin
    try
      if IsParam1(Param) then
      begin
        v := Param[0];
        size := AsInteger(@v);
        //指定サイズを読み込む
        SetLength(s,size);
        FFile.Read(s[1],size);
        Result := BuildString(s);
      end
      else begin
        //全て読む
        SetLength(buf,BUFSIZE);
        repeat
          res := FFile.Read(buf[1],BUFSIZE);
          if res > 0 then
            s := s + Copy(buf,1,res);

        until (res <= 0);
        Result := BuildString(s);
      end;
    except
      raise EJThrow.Create(E_FILE,'read error: ' + FFilename);
    end;
  end
  else
    raise EJThrow.Create(E_FILE,'read error: ' + FFilename);
end;

function TJFileObject.DoReadln(Param: TJValueList): TJValue;
const
  BUFSIZE = 4096;
var
  s,buf: String;
  res,index,currentpos: Integer;
begin
  Result := BuildNull;
  s := '';
  if FFile.IsOpened then
  begin
    try
      //改行まで読む
      SetLength(buf,BUFSIZE);
      repeat
        currentpos := FFile.Position;
        res := FFile.Read(buf[1],BUFSIZE);
        if res > 0 then
        begin
          index := Pos(LF,buf);
          if index > 0 then
          begin
            s := s + Copy(buf,1,index);
            s := TrimRight(s);
            FFile.Position := currentpos + index;
            Break;
          end
          else
            s := s + Copy(buf,1,res);
        end;

      until (res <= 0);

      Result := BuildString(s);
    except
      raise EJThrow.Create(E_FILE,'read error: ' + FFilename);
    end;
  end
  else
    raise EJThrow.Create(E_FILE,'read error: ' + FFilename);
end;

function TJFileObject.DoRemove(Param: TJValueList): TJValue;
//ファイルを削除する
begin
  FFile.Close;
  if DeleteFile(FFilename) then
    Result := BuildBool(True)
  else
    raise EJThrow.Create(E_FILE,'remove error: ' + FFilename);
end;

function TJFileObject.DoRenameTo(Param: TJValueList): TJValue;
//リネーム
var
  v: TJValue;
  s: String;
begin
  FFile.Close;
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    if RenameFile(FFilename,s) then
    begin
      FFilename := s;
      Result := BuildBool(True);
    end
    else
      raise EJThrow.Create(E_FILE,'rename error: ' + FFilename);
  end
  else
    raise EJThrow.Create(E_FILE,'rename error: ' + FFilename);
end;

function TJFileObject.DoSeek(Param: TJValueList): TJValue;
var
  v: TJValue;
  po: Integer;
begin
  if FFile.IsOpened and IsParam1(Param) then
  begin
    v := Param[0];
    po := AsInteger(@v);
    if po < 0 then
      po := FFile.Size + po;

    try
      Result := BuildInteger(FFile.Seek(po,soFromBeginning));
    except
      raise EJThrow.Create(E_FILE,'seek error: ' + FFilename);
    end;
  end
  else
    raise EJThrow.Create(E_FILE,'seek error: ' + FFilename);
end;

function TJFileObject.DoWrite(Param: TJValueList): TJValue;
const
  BUFSIZE = 65535;
var
  s: String;
  res,size,index: Integer;
  v: TJValue;
begin
  Result := BuildNull;
  if FFile.IsOpened then
  begin
    try
      v := Param[0];
      s := AsString(@v);
      index := 1;
      size := system.Length(s);
      res := 0;
      repeat
        if size > 0 then
          res := FFile.Write(s[index],size)
        else
          Break;

        if res > 0 then
        begin
          Inc(index,res);
          Dec(size,res);
        end;
      until (res <= 0);
      //書き込みサイズを返す
      Result := BuildInteger(index - 1);
    except
      raise EJThrow.Create(E_FILE,'write error: ' + FFilename);
    end;
  end
  else
    raise EJThrow.Create(E_FILE,'write error: ' + FFilename);
end;

function TJFileObject.DoWriteln(Param: TJValueList): TJValue;
//改行付き書き込み
var
  v: TJValue;
  s: String;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v) + CRLF;
    Param.Clear;
    Param.Add(BuildString(s));
  end;
  Result := DoWrite(Param);
end;

function TJFileObject.GetLastModified: TJDateObject;
//ファイルの日付を返す
var
  res: Integer;
  date: TDateTime;
begin
  Result := TJDateObject.Create(FEngine);

  res := FileAge(FFilename);
  if res = -1 then
    date := Now
  else
    date := FileDateToDateTime(res);

  Result.LocalTime := date;
end;

function TJFileObject.GetLength: Integer;
//ファイルサイズを返す
begin
  if FFile.IsOpened then
    Result := FFile.Size
  else begin
    //例外
    //raise EJThrow.Create(E_FILE,'length error: ' + FFilename);
    Result := GetFileSize(FFilename);
  end;
end;

function TJFileObject.ToString(Value: PJValue): String;
begin
  Result := FFilename;
end;

(*
  ここから下はファイル操作というよりファイル名操作なのでクラスメソッド的な扱い
  引数があれば引数のファイル名を、無ければFFileNameを対象とする
*)

function TJFileObject.GetTargetFilename(Param: TJValueList): String;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := AsString(@v);
  end
  else
    Result := FFileName;
end;

function TJFileObject.DoExists(Param: TJValueList): TJValue;
//存在する？
begin
  Result := BuildBool(FileExists(GetTargetFilename(Param)));
end;

function TJFileObject.DoChangeExt(Param: TJValueList): TJValue;
//拡張子変更
var
  v: TJValue;
  f,e: String;
begin
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsParam2(Param) then
    begin
      f := AsString(@v);
      v := Param[1];
      e := AsString(@v);
    end
    else begin
      f := FFileName;
      e := AsString(@v);
    end;
    Result := BuildString(ChangeFileExt(f,e));
  end;
end;

function TJFileObject.DoExpandUNCFilename(Param: TJValueList): TJValue;
//UNCに変換
begin
  Result := BuildString(ExpandUNCFileName(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractDir(Param: TJValueList): TJValue;
//ディレクトリ取得(\なし)
begin
  Result := BuildString(ExtractFileDir(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractDrive(Param: TJValueList): TJValue;
//ドライブ取得
begin
  Result := BuildString(ExtractFileDrive(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractExt(Param: TJValueList): TJValue;
//拡張子取得
begin
  Result := BuildString(ExtractFileExt(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractName(Param: TJValueList): TJValue;
//名前部分取得
begin
  Result := BuildString(ExtractFileName(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractPath(Param: TJValueList): TJValue;
//ディレクトリ取得(\つき)
begin
  Result := BuildString(ExtractFilePath(GetTargetFilename(Param)));
end;

function TJFileObject.DoExtractShortPathName(Param: TJValueList): TJValue;
//8.3形式に変換
begin
  Result := BuildString(ExtractShortPathName(GetTargetFilename(Param)));
end;


{ TJDirectoryObject }

constructor TJDirectoryObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('Directory');

  if IsParam1(Param) then
  begin
    v := Param[0];
    SetDirName(AsString(@v));
  end
  else
    SetDirname('.');

  RegistMethod('make',DoMake);
  RegistMethod('remove',DoRemove);
  RegistMethod('renameTo',DoRenameTo);
  RegistMethod('findFirstFile',DoFindFirstFile);
  RegistMethod('findNext',DoFindNext);
  RegistMethod('findNextFile',DoFindNextFile);
  RegistMethod('findNextDir',DoFindNextDir);
  RegistMethod('findFirstDir',DoFindFirstDir);
  RegistMethod('findFirst',DoFindFirst);
  RegistMethod('findClose',DoFindClose);
  RegistMethod('exists',DoExists);
  RegistMethod('changeTo',DoChangeTo);
  RegistMethod('includePathDelimiter',DoIncludePathDelimiter);
  RegistMethod('excludePathDelimiter',DoExcludePathDelimiter);
  RegistMethod('clear',DoClear);
  RegistMethod('findFiles',DoFindFiles);
  RegistMethod('expandUNCFilename',DoExpandUNCFilename);
end;

destructor TJDirectoryObject.Destroy;
begin
  if FBeginFind then
    FindClose(FSearchRec);

  inherited;
end;

function TJDirectoryObject.DoClear(Param: TJValueList): TJValue;
//ファイルを削除する
var
  v: TJValue;
  trash: Boolean;
  handle: THandle;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    trash := AsBool(@v);
  end
  else
    trash := True;

  handle := 0;
{$IFNDEF CONSOLE}
  if Assigned(Application.MainForm) then
    handle := Application.MainForm.Handle;
{$ENDIF}
  ClearFolder(handle,FDirName,trash,False);
end;

function TJDirectoryObject.DoFindClose(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
  if FBeginFind then
    FindClose(FSearchRec);

  FBeginFind := False;
end;

function TJDirectoryObject.DoFindFiles(Param: TJValueList): TJValue;
//wildcardでファイルを探す
var
  sl: TJStringsObject;
begin
  sl := TJStringsObject.Create(FEngine);
  Result := BuildObject(sl);
  FindFiles(FDirname,GetWildcard(Param),fffFile,sl.FStrings);
end;

function TJDirectoryObject.DoFindFirst(Param: TJValueList): TJValue;
begin
  if FBeginFind then
    FindClose(FSearchRec)
  else
    FBeginFind := True;

  if FindFirst(FDirname + GetWildcard(Param),faAnyFile,FSearchRec) = 0 then
  begin
    Result := BuildString(FSearchRec.Name);
  end
  else
    Result := BuildNull;
end;

function TJDirectoryObject.DoFindFirstDir(Param: TJValueList): TJValue;
begin
  if FBeginFind then
    FindClose(FSearchRec)
  else
    FBeginFind := True;

  if FindFirst(FDirname + GetWildcard(Param),faDirectory,FSearchRec) = 0 then
  begin
    Result := BuildString(FSearchRec.Name);
  end
  else
    Result := BuildNull;
end;

function TJDirectoryObject.DoFindFirstFile(Param: TJValueList): TJValue;
begin
  if FBeginFind then
    FindClose(FSearchRec)
  else
    FBeginFind := True;

  if FindFirst(FDirname + GetWildcard(Param),
       faReadOnly or faHidden or faSysFile or faArchive,FSearchRec) = 0 then
  begin
    Result := BuildString(FSearchRec.Name);
  end
  else
    Result := BuildNull;
end;

function TJDirectoryObject.DoFindNext(Param: TJValueList): TJValue;
begin
  if not FBeginFind then
    raise EJThrow.Create(E_DIR,'findNext error: ' + FDirName);

  if FindNext(FSearchRec) = 0 then
  begin
    Result := BuildString(FSearchRec.Name);
  end
  else
    Result := BuildNull;
end;

function TJDirectoryObject.DoFindNextDir(Param: TJValueList): TJValue;
begin
  if not FBeginFind then
    raise EJThrow.Create(E_DIR,'findNextDir error: ' + FDirName);

  Result := BuildNull;
  while FindNext(FSearchRec) = 0 do
  begin
    if (FSearchRec.Attr and faDirectory) <> 0 then
    begin
      //if (FSearchRec.Name = '.') or (FSearchRec.Name = '..') then
      //  Continue
      //else begin
        Result := BuildString(FSearchRec.Name);
        Break;
      //end;
    end
    else
      Continue;
  end;
end;

function TJDirectoryObject.DoFindNextFile(Param: TJValueList): TJValue;
var
  attr: Integer;
begin
  if not FBeginFind then
    raise EJThrow.Create(E_DIR,'findNextFile error: ' + FDirName);

  Result := BuildNull;
  attr := faDirectory or faVolumeID;
  while FindNext(FSearchRec) = 0 do
  begin
    if (FSearchRec.Attr and attr) <> 0 then
    begin
      Continue;
    end
    else begin
      Result := BuildString(FSearchRec.Name);
      Break;
    end;
  end;
end;

function TJDirectoryObject.DoMake(Param: TJValueList): TJValue;
begin
  Result := BuildBool(True);
  try
    if not ForceDirectories(FDirname) then
      raise EJThrow.Create(E_DIR,'make error: ' + FDirName);;
  except
    raise EJThrow.Create(E_DIR,'make error: ' + FDirName);
  end;
end;

function TJDirectoryObject.DoRemove(Param: TJValueList): TJValue;
begin
  Result := BuildBool(True);
  try
    if not RemoveDir(FDirname) then
      raise EJThrow.Create(E_DIR,'remove error: ' + FDirName);
  except
    raise EJThrow.Create(E_DIR,'remove error: ' + FDirName);
  end;
end;

function TJDirectoryObject.DoRenameTo(Param: TJValueList): TJValue;
begin
  //未実装
  EmptyValue(Result);
end;

function TJDirectoryObject.GetDirectories: TJStringsObject;
begin
  Result := TJStringsObject.Create(FEngine);
  EnumFolders(FDirname,Result.FStrings);
end;

function TJDirectoryObject.GetFiles: TJStringsObject;
begin
  Result := TJStringsObject.Create(FEngine);
  FindFiles(FDirname,'*.*',fffFile,Result.FStrings);
end;

function TJDirectoryObject.GetWildCard(Param: TJValueList): String;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := AsString(@v);
  end
  else
    Result := '*.*';
end;

procedure TJDirectoryObject.SetDirname(const Value: String);
begin
  FDirname := IncludeTrailingBackslash(ExpandUNCFileName(Value));
end;

function TJDirectoryObject.ToString(Value: PJValue): String;
begin
  Result := FDirName;
end;

(*
  ここから下はディレクトリ操作というよりディレクトリ名操作なのでクラスメソッド的な扱い
  引数があれば引数のディレクトリ名を、無ければFDirnameを対象とする
*)

function TJDirectoryObject.GetTargetDirname(Param: TJValueList): String;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := AsString(@v);
  end
  else
    Result := FDirname;
end;

function TJDirectoryObject.DoChangeTo(Param: TJValueList): TJValue;
//カレント変更
begin
  EmptyValue(Result);
  ChDir(GetTargetDirname(Param));
end;

function TJDirectoryObject.DoExists(Param: TJValueList): TJValue;
//存在する？
begin
  Result := BuildBool(DirectoryExists(GetTargetDirname(Param)));
end;

function TJDirectoryObject.DoExpandUNCFilename(Param: TJValueList): TJValue;
//UNC変換(TJFileObjectと同じ)
begin
  Result := BuildString(ExpandUNCFileName(GetTargetDirname(Param)));
end;

function TJDirectoryObject.DoIncludePathDelimiter(Param: TJValueList): TJValue;
//末尾を\付きにする
begin
  Result := BuildString(IncludeTrailingBackslash(GetTargetDirname(Param)));
end;

function TJDirectoryObject.DoExcludePathDelimiter(Param: TJValueList): TJValue;
//末尾を\なしにする
begin
  Result := BuildString(ExcludeTrailingBackslash(GetTargetDirname(Param)));
end;


{ TJBaseStringsObject }

constructor TJBaseStringsObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Strings');

  RegistMethod('add',DoAdd);
  RegistMethod('insert',DoInsert);
  RegistMethod('delete',DoDelete);
  RegistMethod('clear',DoClear);
  RegistMethod('indexOf',DoIndxOf);
  RegistMethod('saveToFile',DoSaveToFile);
  RegistMethod('loadFromFile',DoLoadFromFile);
  RegistMethod('assign',DoAssign);
  RegistMethod('addStrings',DoAddStrings);
end;

destructor TJBaseStringsObject.Destroy;
begin
  FStrings := nil;
  inherited;
end;

function TJBaseStringsObject.DoAdd(Param: TJValueList): TJValue;
var
  v: TJValue;
  i: Integer;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  if IsParam1(Param) then
  begin
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      Result := BuildInteger(FStrings.Add(AsString(@v)));
    end;
  end
  else
    Result := BuildInteger(FStrings.Add(''));
end;

function TJBaseStringsObject.DoAddStrings(Param: TJValueList): TJValue;
var
  v: TJValue;
  i: Integer;
  a: TJObject;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsObject(@v) then
    begin
      if v.vObject is TJBaseStringsObject then
        FStrings.AddStrings((v.vObject as TJBaseStringsObject).FStrings)
      else if IsArrayObject(@v) then
      begin
        a := v.vObject;
        for i := 0 to a.GetCount - 1 do
        begin
          v := a.GetItem(i);
          FStrings.Add(AsString(@v))
        end;
      end
      else
        raise EJThrow.Create(E_STRINGS,'addStrings error');
    end
    else
      DoAdd(Param);
  end
  else
    raise EJThrow.Create(E_STRINGS,'addStrings error');
end;

function TJBaseStringsObject.DoAssign(Param: TJValueList): TJValue;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  FStrings.BeginUpdate;
  try
    FStrings.Clear;
    Result := DoAddStrings(Param);
  finally
    FStrings.EndUpdate;
  end;
end;

function TJBaseStringsObject.DoClear(Param: TJValueList): TJValue;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  FStrings.Clear;
end;

function TJBaseStringsObject.DoDelete(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    FStrings.Delete(AsInteger(@v));
  end
  else
    raise EJThrow.Create(E_STRINGS,'delete error');
end;

function TJBaseStringsObject.DoIndxOf(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(FStrings.IndexOf(AsString(@v)));
  end
  else
    Result := BuildInteger(-1);
end;

function TJBaseStringsObject.DoInsert(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
  index: Integer;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    v := Param[0];
    index := AsInteger(@v);
    v := Param[1];
    s := AsString(@v);
    FStrings.Insert(index,s);
  end
  else
    raise EJThrow.Create(E_STRINGS,'insert error');
end;

function TJBaseStringsObject.DoLoadFromFile(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    try
      FStrings.LoadFromFile(s);
    except
      raise EJThrow.Create(E_STRINGS,'loadFromFile error');
    end
  end
  else
    raise EJThrow.Create(E_STRINGS,'loadFromFile error');
end;

function TJBaseStringsObject.DoSaveToFile(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    try
      FStrings.SaveToFile(s);
    except
      raise EJThrow.Create(E_STRINGS,'saveToFile error');
    end
  end
  else
    raise EJThrow.Create(E_STRINGS,'saveToFile error');
end;

function TJBaseStringsObject.GetCommaText: String;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := FStrings.CommaText;
end;

function TJBaseStringsObject.GetItem(Index: Integer): TJValue;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := BuildString(FStrings[Index]);
end;

function TJBaseStringsObject.GetCount: Integer;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := FStrings.Count;
end;

function TJBaseStringsObject.GetText: String;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  Result := FStrings.Text;
end;

function TJBaseStringsObject.GetValue(S: String;
  ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
var
  i: Integer;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  if not ArrayStyle then
    Result := inherited GetValue(S,ArrayStyle)
  else begin
    try
      i := StrToInt(S);
      Result := BuildString(FStrings[i]);
    except
      on EConvertError do
        Result := inherited GetValue(S,ArrayStyle);
      else
        raise EJThrow.Create(E_INDEX,S);
    end;
  end;
end;

procedure TJBaseStringsObject.SetCommaText(const Value: String);
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  FStrings.CommaText := Value;
end;

procedure TJBaseStringsObject.SetText(const Value: String);
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  FStrings.Text := Value;
end;

procedure TJBaseStringsObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
var
  i: Integer;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  if ArrayStyle then
  begin
    try
      i := StrToInt(S);
      FStrings[i] := AsString(@Value);
    except
      on EConvertError do
        inherited SetValue(S,Value,ArrayStyle);
      else
        raise EJThrow.Create(E_INDEX,S);
    end;
  end
  else
    inherited SetValue(S,Value,ArrayStyle);
end;

function TJBaseStringsObject.ToString(Value: PJValue): String;
var
  sepa: String;
begin
  if not Assigned(FStrings) then
    raise EJThrow.Create(E_STRINGS,'strings is null');

  if Assigned(Value) then
  begin
    sepa := AsString(Value);
    Result := StringReplace(FStrings.Text,CRLF,sepa,[rfReplaceAll]);
  end
  else
    Result := FStrings.Text;
end;

class function TJBaseStringsObject.IsArray: Boolean;
begin
  Result := True;
end;

{ TJWin32Object }

constructor TJWin32Object.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Win32');

  RegistMethod('sleep',DoSleep);
  RegistMethod('winExec',DoWinExec);
  RegistMethod('shellExecute',DoShellExecute);
  RegistMethod('getTickCount',DoGetTickCount);
end;

destructor TJWin32Object.Destroy;
begin
  inherited;
end;

function TJWin32Object.DoGetTickCount(Param: TJValueList): TJValue;
begin
  Result := BuildInteger(GetTickCount);
end;

function TJWin32Object.DoShellExecute(Param: TJValueList): TJValue;
var
  v: TJValue;
  op,name,args,dir: String;
begin
  Result := BuildBool(False);

  if IsParam1(Param) then
  begin
    args := '';
    dir := '';

    v := Param[0];
    op := AsString(@v);
    if IsParam2(Param) then
    begin
      v := Param[1];
      name := AsString(@v);
      if IsParam3(Param) then
      begin
        v := Param[2];
        args := AsString(@v);
        if IsParam4(Param) then
        begin
          v := Param[3];
          dir := AsString(@v);
        end;
      end;

      //実行
      Result := BuildBool(ShellExecute(0,PChar(op),PChar(name),PChar(args),PChar(dir),SW_SHOW) > 31);
    end
    else
      raise EJThrow.Create(E_WIN32,'shellExecute error');
  end
  else
    raise EJThrow.Create(E_WIN32,'shellExecute error');
end;

function TJWin32Object.DoSleep(Param: TJValueList): TJValue;
//sleep
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Sleep(AsInteger(@v));
  end
  else
    Sleep(0);
end;

function TJWin32Object.DoWinExec(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    Result := BuildBool(WinExec(PChar(s),SW_SHOW) > 31);
  end
  else
    raise EJThrow.Create(E_WIN32,'winExec error');
end;

{ TJIniObject }

constructor TJIniObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('Ini');

  RegistMethod('deleteKey',DoDeleteKey);
  RegistMethod('eraseSection',DoEraseSection);
  RegistMethod('readSection',DoReadSection);
  RegistMethod('readSections',DoReadSections);
  RegistMethod('sectionExists',DoSectionExists);
  RegistMethod('write',DoWrite);
  RegistMethod('read',DoRead);
  RegistMethod('update',DoUpdate);

  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      try
        FIni := TMemIniFile.Create(AsString(@v));
      except
        raise EJThrow.Create(E_INI,'ini create error');
      end;
    end
    else
      raise EJThrow.Create(E_INI,'ini create error');
  except

    raise;
  end;
end;

destructor TJIniObject.Destroy;
begin
  FreeAndNil(FIni);
  inherited;
end;

function TJIniObject.DoDeleteKey(Param: TJValueList): TJValue;
var
  v1,v2: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    FIni.DeleteKey(AsString(@v1),AsString(@v2));
  end
  else
    raise EJThrow.Create(E_INI,'deleteKey error: ' + FIni.Filename);
end;

function TJIniObject.DoEraseSection(Param: TJValueList): TJValue;
var
  v1: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v1 := Param[0];
    try
      FIni.EraseSection(AsString(@v1));
    except
      raise EJThrow.Create(E_INI,'eraseSection error: ' + FIni.Filename);
    end;
  end
  else
    raise EJThrow.Create(E_INI,'eraseSection error: ' + FIni.Filename);
end;

function TJIniObject.DoRead(Param: TJValueList): TJValue;
var
  v1,v2,v3: TJValue;
begin
  EmptyValue(Result);
  if IsParam3(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    v3 := Param[2];
    Result := BuildString(FIni.ReadString(AsString(@v1),AsString(@v2),AsString(@v3)));
  end
  else
    raise EJThrow.Create(E_INI,'read error: ' + FIni.Filename);
end;

function TJIniObject.DoReadSection(Param: TJValueList): TJValue;
var
  v1: TJValue;
  sl: TStringList;
  ss: TJStringsObject;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v1 := Param[0];
    sl := TStringList.Create;
    try try
      FIni.ReadSection(AsString(@v1),sl);
      ss := TJStringsObject.Create(FEngine);
      ss.FStrings.Assign(sl);
      Result := BuildObject(ss);
    except
      raise EJThrow.Create(E_INI,'readSection error: ' + FIni.Filename);
    end;
    finally
      sl.Free;
    end;
  end
  else
    raise EJThrow.Create(E_INI,'readSection error: ' + FIni.Filename);

end;

function TJIniObject.DoReadSections(Param: TJValueList): TJValue;
var
  sl: TStringList;
  ss: TJStringsObject;
begin
  EmptyValue(Result);
  sl := TStringList.Create;
  try try
    FIni.ReadSections(sl);
    ss := TJStringsObject.Create(FEngine);
    ss.FStrings.Assign(sl);
    Result := BuildObject(ss);
  except
    raise EJThrow.Create(E_INI,'readSections error: ' + FIni.Filename);
  end;
  finally
    sl.Free;
  end;

end;

function TJIniObject.DoSectionExists(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(FIni.SectionExists(AsString(@v)));
  end
  else
    Result := BuildBool(False);
end;

function TJIniObject.DoUpdate(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  FIni.UpdateFile;
end;

function TJIniObject.DoWrite(Param: TJValueList): TJValue;
var
  v1,v2,v3: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam3(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    v3 := Param[2];
    FIni.WriteString(AsString(@v1),AsString(@v2),AsString(@v3));
    //FIni.UpdateFile;
  end
  else
    raise EJThrow.Create(E_INI,'write error: ' + FIni.Filename);
end;

function TJIniObject.GetFilename: String;
begin
  Result := FIni.FileName;
end;

class function TJIniObject.IsMakeGlobalInstance: Boolean;
begin
  Result := False;
end;

{ TJCRCObject }

function TJCRCObject.DoCalc(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    CalcStringCRC(FCRC16,FCRC32,AsString(@v));
  end
  else
    raise EJThrow.Create(E_CRC,'calc error');
end;

function TJCRCObject.DoCalcFile(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if not CalcFileCRC(FCRC16,FCRC32,AsString(@v)) then
      raise EJThrow.Create(E_CRC,'calcFile error');
  end
  else
    raise EJThrow.Create(E_CRC,'calcFile error');
end;

constructor TJCRCObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('CRC');

  FCRC16 := $FFFF;
  FCRC32 := $FFFFFFFF;
  RegistMethod('calc',DoCalc);
  RegistMethod('calcFile',DoCalcFile);
end;

function TJCRCObject.GetCRC16: Integer;
begin
  Result := FCRC16;
end;

function TJCRCObject.GetCRC32: Integer;
begin
  Result := FCRC32;
end;

{ TJBase64Object }

constructor TJBase64Object.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Base64');

  RegistMethod('encode',DoEncode);
  RegistMethod('decode',DoDecode);
  RegistMethod('encodeHeader',DoEncodeHeader);
  RegistMethod('decodeHeader',DoDecodeHeader);
end;

function TJBase64Object.DoDecode(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(DecodeBase64(AsString(@v)));
  end
  else
    raise EJThrow.Create(E_BASE64,'decode error');
end;

function TJBase64Object.DoDecodeHeader(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(jis2sjis(DecodeHeaderString(AsString(@v))));
  end
  else
    raise EJThrow.Create(E_BASE64,'decodeHeader error');
end;

function TJBase64Object.DoEncode(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(EncodeBase64(AsString(@v)));
  end
  else
    raise EJThrow.Create(E_BASE64,'encode error');
end;

function TJBase64Object.DoEncodeHeader(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(CreateHeaderString(AsString(@v)));
  end
  else
    raise EJThrow.Create(E_BASE64,'encodeHeader error');
end;

{ TJDialogObject }

constructor TJDialogObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
  i: Integer;
begin
  inherited;
  RegistName('Dialog');

  FFilters := TJStringsObject.Create(FEngine,nil,False);
  FFilters.IncRef;
  if IsParam1(Param) then
  begin
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      FFilters.Strings.Add(AsString(@v));
    end;
  end;
  RegistMethod('openFile',DoOpenFile);
  RegistMethod('openFiles',DoOpenFiles);
  RegistMethod('saveFile',DoSaveFile);
  RegistMethod('openFolder',DoOpenFolder);
end;

destructor TJDialogObject.Destroy;
begin
  FFilters.DecRef;
  inherited;
end;

function TJDialogObject.GetFilter: string;
var
  i: Integer;
begin
  if FFilters.Strings.Count > 0 then
  begin
    Result := Trim(FFilters.Strings[0]);
    for i := 1 to FFilters.Strings.Count - 1 do
      Result := Result + '|' + Trim(FFilters.Strings[i]);
  end
  else
    Result := '';
end;

function TJDialogObject.DoOpenFile(Param: TJValueList): TJValue;
//open dialogを開く
var
  dialog: TOpenDialog;
  v: TJValue;
  title,default: String;
begin
  Result := BuildNull;
  title := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  default := '';
  //タイトル
  if IsParam1(Param) then
  begin
    v := Param[0];
    title := AsString(@v);
  end;
  //デフォルトファイル名
  if IsParam2(Param) then
  begin
    v := Param[1];
    default := AsString(@v);
  end;

  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := title;
    dialog.FileName := default;
    dialog.Filter := GetFilter;
    if dialog.Execute then
      Result := BuildString(dialog.Filename);
  finally
    dialog.Free;
  end;
end;

function TJDialogObject.DoOpenFiles(Param: TJValueList): TJValue;
//open dialogを開いて文字列リストを返す
var
  dialog: TOpenDialog;
  v: TJValue;
  title,default: String;
  sl: TJStringsObject;
begin
  sl := TJStringsObject.Create(FEngine);
  Result := BuildObject(sl);

  title := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  default := '';
  //タイトル
  if IsParam1(Param) then
  begin
    v := Param[0];
    title := AsString(@v);
  end;
  //デフォルトファイル名
  if IsParam2(Param) then
  begin
    v := Param[1];
    default := AsString(@v);
  end;

  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := title;
    dialog.FileName := default;
    dialog.Filter := GetFilter;
    dialog.Options := dialog.Options + [ofAllowMultiSelect];
    //複数のファイル名
    if dialog.Execute then
      sl.FStrings.Assign(dialog.Files);
  finally
    dialog.Free;
  end;
end;

function TJDialogObject.DoOpenFolder(Param: TJValueList): TJValue;
//folderを開く
var
  v: TJValue;
  title,dir: String;
  h: THandle;
begin
  Result := BuildNull;
  title := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  dir := '';
  //タイトル
  if IsParam1(Param) then
  begin
    v := Param[0];
    title := AsString(@v);
  end;
  //デフォルトdir名
  if IsParam2(Param) then
  begin
    v := Param[1];
    dir := AsString(@v);
  end
  else
    dir := GetCurrentDir;

  h := 0;
{$IFNDEF CONSOLE}
  if Assigned(Application.MainForm) then
    h := Application.MainForm.Handle;
{$ENDIF}
  if SelectFolder(h,title,'',dir) then
    Result := BuildString(dir);
end;

function TJDialogObject.DoSaveFile(Param: TJValueList): TJValue;
//save dialogを開く
var
  dialog: TSaveDialog;
  v: TJValue;
  title,default: String;
begin
  Result := BuildNull;
  title := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  default := '';
  //タイトル
  if IsParam1(Param) then
  begin
    v := Param[0];
    title := AsString(@v);
  end;
  //デフォルトファイル名
  if IsParam2(Param) then
  begin
    v := Param[1];
    default := AsString(@v);
  end;

  dialog := TSaveDialog.Create(nil);
  try
    dialog.Title := title;
    dialog.FileName := default;
    dialog.Filter := GetFilter;
    if dialog.Execute then
      Result := BuildString(dialog.Filename);
  finally
    dialog.Free;
  end;
end;

{ TJMutexObject }

constructor TJMutexObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  s: String;
  v: TJValue;
begin
  inherited;
  RegistName('Mutex');

  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
  end
  else
    s := '';

  FMutex := TMutex.Create(s);

  RegistMethod('lock',DoLock);
  RegistMethod('unlock',DoUnLock);
  RegistMethod('tryLock', DoTryLock);
  RegistMethod('exists', DoExists);
end;

destructor TJMutexObject.Destroy;
begin
  FreeAndNil(FMutex);
  inherited;
end;

function TJMutexObject.DoLock(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  FMutex.Lock;
end;

function TJMutexObject.DoTryLock(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FMutex.TryLock);
end;

function TJMutexObject.DoUnLock(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  FMutex.UnLock;
end;

function TJMutexObject.DoExists(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FMutex.Existed);
end;

function TJMutexObject.GetExisted: Boolean;
begin
  Result := FMutex.Existed;
end;

function TJMutexObject.GetTimeout: Integer;
begin
  Result := FMutex.Timeout;
end;

procedure TJMutexObject.SetTimeout(const Value: Integer);
begin
  FMutex.Timeout := Value;
end;

{ TJKeyboard }

constructor TJKeyboard.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Keyboard');
  //メソッド登録
  RegistMethod('isDown',DoIsDown);
end;

function TJKeyboard.DoIsDown(Param: TJValueList): TJValue;
//keyが押されているかどうか？
var
  v: TJValue;
  c: Char;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    c := AsChar(@v);
    if c <> #0 then
      Result := BuildBool(GetAsyncKeyState(Integer(c)) < 0);
  end;
end;

function TJKeyboard.GetAlt: Boolean;
begin
  Result := GetAsyncKeyState(VK_MENU) < 0;
end;

function TJKeyboard.GetCtrl: Boolean;
begin
  Result := GetAsyncKeyState(VK_CONTROL) < 0;
end;

function TJKeyboard.GetShift: Boolean;
begin
  Result := GetAsyncKeyState(VK_SHIFT) < 0;
end;


{ TJMouse }

constructor TJMouse.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Mouse');
end;

function TJMouse.GetLButton: Boolean;
begin
  Result := GetAsyncKeyState(VK_LBUTTON) < 0;
end;

function TJMouse.GetMButton: Boolean;
begin
  Result := GetAsyncKeyState(VK_MBUTTON) < 0;
end;

function TJMouse.GetRButton: Boolean;
begin
  Result := GetAsyncKeyState(VK_RBUTTON) < 0;
end;

function TJMouse.GetX: Integer;
var
  p: TPoint;
begin
  GetCursorPos(p);
  Result := p.X;
end;

function TJMouse.GetY: Integer;
var
  p: TPoint;
begin
  GetCursorPos(p);
  Result := p.Y;
end;


{ TJClipboard }

constructor TJClipboard.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Clipboard');
  RegistMethod('hasText',DoHasText);
end;

function TJClipboard.DoHasText(Param: TJValueList): TJValue;
begin
  Result := BuildBool(Clipboard.HasFormat(CF_TEXT));
end;

function TJClipboard.GetAsText: String;
begin
  Result := '';
  if Clipboard.HasFormat(CF_TEXT) then
    Result := Clipboard.AsText;
end;

procedure TJClipboard.SetAsText(const Value: String);
begin
  Clipboard.AsText := Value;
end;

function TJClipboard.ToString(Value: PJValue): String;
begin
  Result := GetAsText;
end;

{ TJRegIniObject }

constructor TJRegIniObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('RegIni');

  RegistMethod('deleteKey',DoDeleteKey);
  RegistMethod('eraseSection',DoEraseSection);
  RegistMethod('readSection',DoReadSection);
  RegistMethod('readSections',DoReadSections);
  RegistMethod('sectionExists',DoSectionExists);
  RegistMethod('write',DoWrite);
  RegistMethod('read',DoRead);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      try
        FReg := TRegIniFile.Create(AsString(@v));
        FReg.RootKey := HKEY_CURRENT_USER;
      except
        raise EJThrow.Create(E_INI,'ini create error');
      end;
    end
    else
      raise EJThrow.Create(E_INI,'ini create error');
  except

    raise;
  end;
end;

destructor TJRegIniObject.Destroy;
begin
  FreeAndNil(FReg);
  inherited;
end;

function TJRegIniObject.DoDeleteKey(Param: TJValueList): TJValue;
var
  v1,v2: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    FReg.DeleteKey(AsString(@v1),AsString(@v2));
  end
  else
    raise EJThrow.Create(E_INI,'deleteKey error: ' + FReg.Filename);
end;

function TJRegIniObject.DoEraseSection(Param: TJValueList): TJValue;
var
  v1: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v1 := Param[0];
    try
      FReg.EraseSection(AsString(@v1));
    except
      raise EJThrow.Create(E_INI,'eraseSection error: ' + FReg.Filename);
    end;
  end
  else
    raise EJThrow.Create(E_INI,'eraseSection error: ' + FReg.Filename);
end;

function TJRegIniObject.DoRead(Param: TJValueList): TJValue;
var
  v1,v2,v3: TJValue;
begin
  EmptyValue(Result);
  if IsParam3(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    v3 := Param[2];
    Result := BuildString(FReg.ReadString(AsString(@v1),AsString(@v2),AsString(@v3)));
  end
  else
    raise EJThrow.Create(E_INI,'read error: ' + FReg.Filename);
end;

function TJRegIniObject.DoReadSection(Param: TJValueList): TJValue;
var
  v1: TJValue;
  sl: TStringList;
  ss: TJStringsObject;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v1 := Param[0];
    sl := TStringList.Create;
    try try
      FReg.ReadSection(AsString(@v1),sl);
      ss := TJStringsObject.Create(FEngine);
      ss.FStrings.Assign(sl);
      Result := BuildObject(ss);
    except
      raise EJThrow.Create(E_INI,'readSection error: ' + FReg.Filename);
    end;
    finally
      sl.Free;
    end;
  end
  else
    raise EJThrow.Create(E_INI,'readSection error: ' + FReg.Filename);

end;

function TJRegIniObject.DoReadSections(Param: TJValueList): TJValue;
var
  sl: TStringList;
  ss: TJStringsObject;
begin
  EmptyValue(Result);
  sl := TStringList.Create;
  try try
    FReg.ReadSections(sl);
    ss := TJStringsObject.Create(FEngine);
    ss.FStrings.Assign(sl);
    Result := BuildObject(ss);
  except
    raise EJThrow.Create(E_INI,'readSections error: ' + FReg.Filename);
  end;
  finally
    sl.Free;
  end;
end;

function TJRegIniObject.DoSectionExists(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(FReg.KeyExists(AsString(@v)));
  end
  else
    Result := BuildBool(False);
end;

function TJRegIniObject.DoWrite(Param: TJValueList): TJValue;
var
  v1,v2,v3: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam3(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    v3 := Param[2];
    FReg.WriteString(AsString(@v1),AsString(@v2),AsString(@v3));
  end
  else
    raise EJThrow.Create(E_INI,'write error: ' + FReg.Filename);
end;

function TJRegIniObject.GetKey: String;
begin
  Result := FReg.FileName;
end;

function TJRegIniObject.GetRootKey: String;
begin
  Result := 'HKEY_CURRENT_USER';
end;

class function TJRegIniObject.IsMakeGlobalInstance: Boolean;
begin
  Result := False;
end;

{ TJStringBufferObject }

constructor TJStringBufferObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
  c: Char;
begin
  inherited;
  RegistName('StringBuffer');
  RegistMethod('append',DoAppend);
  RegistMethod('charCodeAt',DoCharCodeAt);
  RegistMethod('delete',DoDelete);
  RegistMethod('indexOf',DoIndexOf);
  RegistMethod('insert',DoInsert);
  RegistMethod('lastIndexOf',DoLastIndexOf);
  RegistMethod('replace',DoReplace);
  RegistMethod('slice',DoSlice);
  RegistMethod('substr',DoSubStr);
  RegistMethod('substring',DoSubString);
  RegistMethod('fill',DoFill);

  if IsParam1(Param) then
  begin
    v := Param[0];
    //計算結果はDoubleになることが多いためDoubleも許可する
    if IsNumber(@v) then
    begin
      SetLength(AsInteger(@v));
      if system.Length(FBuffer) > 0 then
      begin
        if IsParam2(Param) then
        begin
          v := Param[1];
          c := AsChar(@v);
        end
        else
          c := #0;

        FillChar(FBuffer[1],system.Length(FBuffer),c);
      end;
    end
    else
      FBuffer := AsString(@v);
  end;
end;

function TJStringBufferObject.DoAppend(Param: TJValueList): TJValue;
var
  v: TJValue;
  i: Integer;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      FBuffer := FBuffer + AsString(@v);
    end;
end;

function TJStringBufferObject.DoDelete(Param: TJValueList): TJValue;
var
  start,last,range: Integer;
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    //開始位置
    v := Param[0];
    start := AsInteger(@v);
    //終了位置
    if IsParam2(Param) then
    begin
      v := Param[1];
      last := AsInteger(@v);
    end
    else
      last := MaxInt;
    //範囲
    range := last - start;
    //削除
    if range > 0 then
      system.Delete(FBuffer,start + 1,range);
  end
  else
    raise EJThrow.Create(E_STRING,'delete error');
end;

function TJStringBufferObject.DoInsert(Param: TJValueList): TJValue;
//挿入
var
  index: Integer;
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    //開始位置
    v := Param[0];
    index := AsInteger(@v);
    //挿入文字
    v := Param[1];
    system.Insert(AsString(@v),FBuffer,index + 1);
  end
  else
    raise EJThrow.Create(E_STRING,'insert error');
end;

function TJStringBufferObject.DoIndexOf(Param: TJValueList): TJValue;
//文字を検索
var
  v: TJValue;
  sub: String;
  start: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    sub := AsString(@v);

    if IsParam2(Param) then
    begin
      v := Param[1];
      start := AsInteger(@v) + 1;
      //末尾以上なら末尾として処理
      if start > system.Length(FBuffer) then
        start := system.Length(FBuffer)
    end
    else
      start := 1;

    Result := BuildInteger(IndexOf(sub,FBuffer,start) - 1);
  end
  else
    Result := BuildInteger(-1);
end;

function TJStringBufferObject.DoLastIndexOf(Param: TJValueList): TJValue;
//文字を逆から検索
var
  v: TJValue;
  sub: String;
  start: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    sub := AsString(@v);

    if IsParam2(Param) then
    begin
      v := Param[1];
      start := AsInteger(@v) + 1;
      //0以下なら0として処理
      if start < 1 then
        start := 1;
    end
    else
      //右端から
      start := 0;

    Result := BuildInteger(LastIndexOf(sub,FBuffer,start) - 1);
  end
  else
    Result := BuildInteger(-1);
end;

function TJStringBufferObject.DoReplace(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  if IsParam3(Param) then
  begin
    //範囲を削除
    DoDelete(Param);
    //2番目を消す
    Param.Delete(1);
    //挿入
    DoInsert(Param);
  end
  else
    raise EJThrow.Create(E_STRING,'replace error');
end;

function TJStringBufferObject.DoSlice(Param: TJValueList): TJValue;
//文字列を部分コピー
var
  v: TJValue;
  start,last,len: Integer;
  sb: TJStringBufferObject;
begin
  //slice()はオブジェクトを返す
  sb := TJStringBufferObject.Create(FEngine);

  len := system.Length(FBuffer);
  if IsParam1(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v) + 1;
    if start < 1 then
      start := len + start;

    if IsParam2(Param) then
    begin
      v := Param[1];
      last := AsInteger(@v) + 1;
      if last < 1 then
        last := len + last;
    end
    else
      last := len + start;

    sb.text := Copy(FBuffer,start,last - start);
  end;

  Result := BuildObject(sb);
end;

function TJStringBufferObject.DoSubStr(Param: TJValueList): TJValue;
//文字列をコピー
var
  v: TJValue;
  start,len: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v) + 1;

    if IsParam2(Param) then
    begin
      v := Param[1];
      len := AsInteger(@v);
    end
    else
      len := MaxInt;

    Result := BuildString(Copy(FBuffer,start,len));
  end
  else
    Result := BuildString('');
end;

function TJStringBufferObject.DoSubString(Param: TJValueList): TJValue;
var
  v: TJValue;
  start,last,i,j: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v) + 1;
    if i < 1 then
      i := 1;

    if IsParam2(Param) then
    begin
      v := Param[1];
      j := AsInteger(@v) + 1;
      if j < 1 then
        j := 1;
    end
    else
      j := MaxInt;

    start := Min(i,j);
    last  := Max(i,j);
    Result := BuildString(Copy(FBuffer,start,last - start));
  end
  else
    Result := BuildString('');
end;

function TJStringBufferObject.DoFill(Param: TJValueList): TJValue;
var
  v: TJValue;
  c: Char;
begin
  Result := BuildObject(Self);

  if system.Length(FBuffer) > 0 then
  begin
    if IsParam1(Param) then
    begin
      v := Param[0];
      c := AsChar(@v);
    end
    else
      c := #0;

    FillChar(FBuffer[1],system.Length(FBuffer),c);
  end;
end;

function TJStringBufferObject.GetStringIndex(S: String): Integer;
var
  len: Integer;
begin
  try
    Result := StrToInt(S);
    len := system.Length(FBuffer);
    if Result < 0 then
      Result := len + Result;

    if (Result >= 0) and (Result < len) then
      Result := Result + 1
    else
      raise EJThrow.Create(E_INDEX,S);
  except
    on EConvertError do
      raise EJThrow.Create(E_INDEX,S);
  end;
end;

function TJStringBufferObject.GetLength: Integer;
begin
  Result := system.Length(FBuffer);
end;

function TJStringBufferObject.GetValue(S: String;
  ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
begin
  //配列式の場合
  if ArrayStyle then
  begin
    try
      Result := BuildString(FBuffer[GetStringIndex(S)])
    except
      raise EJThrow.Create(E_INDEX,S);
    end;
  end
  else
    Result := inherited GetValue(S,ArrayStyle);
end;

procedure TJStringBufferObject.SetLength(const Value: Integer);
//文字列の長さをセット
begin
  if Value > -1 then
    System.SetLength(FBuffer,Value)
  else
    raise EJThrow.Create(E_STRING,'length error ' + IntToStr(Value));
end;

procedure TJStringBufferObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
begin
 //配列式の場合
  if ArrayStyle then
  begin
    try
      FBuffer[GetStringIndex(S)] := AsChar(@Value);
    except
      raise EJThrow.Create(E_INDEX,S);
    end;
  end
  else
    inherited;
end;

function TJStringBufferObject.ToString(Value: PJValue): String;
begin
  Result := FBuffer;
end;

function TJStringBufferObject.DoCharCodeAt(Param: TJValueList): TJValue;
//文字コードを返す
var
  v: TJValue;
  index: Integer;
begin
  Result := BuildNaN;
  if IsParam1(Param) then
  begin
    v := Param[0];
    index := AsInteger(@v) + 1;
    //範囲外でも例外が発生しない場合があるので範囲チェックする
    if (index > 0) and (index <= system.Length(FBuffer)) then
      Result := BuildInteger(Integer(FBuffer[index]));
  end;
end;

function TJStringBufferObject.GetCount: Integer;
begin
  Result := GetLength;
end;

function TJStringBufferObject.GetItem(Index: Integer): TJValue;
begin
  Result := BuildString(FBuffer[index]);
end;

class function TJStringBufferObject.IsArray: Boolean;
begin
  Result := True;
end;

{ TJStringsObject }

constructor TJStringsObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  FStrings := TStringList.Create;

  RegistName('StringList');
  RegistMethod('sort',DoSort);

  //初期文字列
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsString(@v) then
      FStrings.Text := AsString(@v);
  end;
end;

destructor TJStringsObject.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

function TJStringsObject.DoSort(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  Strings.Sort;
end;

function TJStringsObject.GetStrings: TStringList;
begin
  Result := FStrings as TStringList;
end;

function TJStringsObject.GetSorted: Boolean;
begin
  Result := Strings.Sorted;
end;

function TJStringsObject.GetCaseSensitive: Boolean;
begin
  Result := Strings.CaseSensitive;
end;

function TJStringsObject.GetDuplicates: Boolean;
begin
  Result := Strings.Duplicates = dupAccept;
end;

procedure TJStringsObject.SetSorted(const Value: Boolean);
begin
  Strings.Sorted := Value;
end;

procedure TJStringsObject.SetCaseSensitive(const Value: Boolean);
begin
  Strings.CaseSensitive := Value;
end;

procedure TJStringsObject.SetDuplicates(const Value: Boolean);
begin
  //trueなら重複を許可
  if Value then
    Strings.Duplicates := dupAccept
  else
    Strings.Duplicates := dupIgnore;
end;

{ TJRect }

constructor TJRect.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Rect');
  FTopLeft := TJPoint.Create(FEngine,nil,False);
  FTopLeft.IncRef;
  FBottomRight := TJPoint.Create(FEngine,nil,False);
  FBottomRight.IncRef;
end;

destructor TJRect.Destroy;
begin
  FTopLeft.DecRef;
  FBottomRight.DecRef;
  inherited;
end;

function TJRect.GetBottom: Integer;
begin
  Result := FBottomRight.y;
end;

function TJRect.GetBottomRight: TJObject;
begin
  Result := FBottomRight;
end;

function TJRect.GetLeft: Integer;
begin
  Result := FTopLeft.x;
end;

function TJRect.Get__Rect: TRect;
begin
  Result.TopLeft := FTopLeft.__Point;
  Result.BottomRight := FBottomRight.__Point;
end;

function TJRect.GetRight: Integer;
begin
  Result := FBottomRight.x;
end;

function TJRect.GetTop: Integer;
begin
  Result := FTopLeft.y;
end;

function TJRect.GetTopLeft: TJObject;
begin
  Result := FTopLeft;
end;

procedure TJRect.SetBottom(const Value: Integer);
begin
  FBottomRight.y := Value;
end;

procedure TJRect.SetLeft(const Value: Integer);
begin
  FTopLeft.x := Value;
end;

procedure TJRect.Set__Rect(const Value: TRect);
begin
  FTopLeft.__Point := Value.TopLeft;
  FBottomRight.__Point := Value.BottomRight;
end;

procedure TJRect.SetRight(const Value: Integer);
begin
  FBottomRight.x := Value;
end;

procedure TJRect.SetTop(const Value: Integer);
begin
  FTopLeft.y := Value;
end;

function TJRect.ToString(Value: PJValue): String;
begin
  Result := Format(
    '{left:%d,top:%d,right:%d,bottom:%d}',[left,top,right,bottom]);
end;

function TJRect.GetRect: String;
begin
  Result := ToString;
end;

procedure TJRect.SetRect(const Value: String);
//文字から直す {left:0,top:0,right:0,bottom:0}
var
  re: TJRegExp;
  inp: String;
begin
  re := TJRegExp.Create;
  try
    inp := StringReplace(Value,' ','',[rfReplaceAll]);
    re.ignoreCase := True;
    re.source :=
      '\{left\:([0-9]+)\,top\:([0-9]+)\,right\:([0-9]+)\,bottom\:([0-9]+)\}';
    if re.Test(inp) and (Length(re.SubMatch) = 5) then
    begin
      left := StrToIntDef(re.SubMatch[1],0);
      top := StrToIntDef(re.SubMatch[2],0);
      right := StrToIntDef(re.SubMatch[3],0);
      bottom := StrToIntDef(re.SubMatch[4],0);
    end;
  finally
    re.Free;
  end;
end;

procedure TJRect.SetBottomRight(const Value: TJObject);
var
  v: TJValue;
begin
  if Assigned(Value) then
  begin
    v := Value.GetValue('x',True);
    FBottomRight.x := AsInteger(@v);
    v := Value.GetValue('y',True);
    FBottomRight.y := AsInteger(@v);
  end;
end;

procedure TJRect.SetTopLeft(const Value: TJObject);
var
  v: TJValue;
begin
  if Assigned(Value) then
  begin
    v := Value.GetValue('x',True);
    FTopLeft.x := AsInteger(@v);
    v := Value.GetValue('y',True);
    FTopLeft.y := AsInteger(@v);
  end;
end;


{ TJPoint }

constructor TJPoint.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Point');
end;

function TJPoint.GetPt: String;
begin
  Result := ToString;
end;

function TJPoint.GetX: Integer;
begin
  Result := __Point.x;
end;

function TJPoint.GetY: Integer;
begin
  Result := __Point.y;
end;

procedure TJPoint.SetPt(const Value: String);
//文字から直す{x:0,y:0}
var
  re: TJRegExp;
  inp: String;
begin
  re := TJRegExp.Create;
  try
    inp := StringReplace(Value,' ','',[rfReplaceAll]);
    re.ignoreCase := True;
    re.source := '\{x\:([0-9]+)\,y\:([0-9]+)\}';
    if re.Test(inp) and (Length(re.SubMatch) = 3) then
    begin
      x := StrToIntDef(re.SubMatch[1],0);
      y := StrToIntDef(re.SubMatch[2],0);
    end;
  finally
    re.Free;
  end;
end;

procedure TJPoint.SetX(const Value: Integer);
begin
  __Point.x := Value;
end;

procedure TJPoint.SetY(const Value: Integer);
begin
  __Point.y := Value;
end;

function TJPoint.ToString(Value: PJValue): String;
begin
  Result := Format('{x:%d,y:%d}',[x,y]);
end;


end.
