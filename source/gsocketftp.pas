unit gSocketFtp;
{
 TgFTP: FTP基本クラス
   Author: Wolfy
 Modified: 00/05/10
  Version: 0.00
}


interface

uses
  Windows,SysUtils,Classes,SyncObjs,gSocket,gSocketMisc,regexpr,misc,
  myclasses,hashtable
{$IFDEF WS2}
  ,winsock2;
{$ELSE}
  ,Winsock;
{$ENDIF}

type
  TVendorType = (vtAUTO,vtUNIX,vtDOS);
  TModeType = (MODE_ASCII,MODE_IMAGE,MODE_BYTE,MODE_UNKNOWN);
  TFTPCmdType = (cmdChangeDir,cmdMakeDir,cmdDelete,cmdRemoveDir,
              cmdList,cmdRename,cmdUpRestore,cmdDownRestore,
              cmdDownload,cmdUpload,cmdAppend,cmdReInit,cmdAllocate,
              cmdNList,cmdDoCommand,cmdCurrentDir,cmdAbort,cmdMode,
              cmdSIZE);
  TFileType = (ftNone, ftDir, ftFile, ftLink);
  //event
  TFailureEvent = procedure(var Handled: Boolean; Trans_Type: TFTPCmdType) of object;
  TSuccessEvent = procedure(Trans_Type: TFTPCmdType) of object;
  TListItemEvent = procedure(Listing: String) of object;
  //ftp
  TUnixFileMode = record
    Read,
    Write,
    Execute: Boolean;
  end;

const
  EmptyUnixFileMode: TUnixFileMode = (
    Read: False;
    Write: False;
    Execute: False);

type
  TUnixFileModes = record
    User,
    Group,
    Other: TUnixFileMode;
  end;

  //FTPファイルのプロパティ
  PFtpFileProperty = ^TFtpFileProperty;
  TFTPFileProperty = record
    FileType: TFileType;
    Size: Integer;
    Name: String;
    ModifiDate: TDateTime;
    Attribute: TUnixFileModes;
    Line: String;
  end;


procedure ClearFtpFileProperty(var Item: TFtpFileProperty);
function GetFtpFileAttr(var Item: TFtpFileProperty): Integer;

type
  //ftp directory list items
  TFTPDirectoryList = class(TObject)
  protected
    FItems: TListPlus;
    FVendor: TVendorType;
    FNames: TPointerHashTable;

    function Get(Index: Integer): PFTPFileProperty;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure ParseLine(Line: String); virtual;
    procedure Assign(Source: TFTPDirectoryList); virtual;
    function NameExists(FileName: String): Boolean;
    function Add(var Item: TFtpFileProperty): Integer;

    property Vendor: TVendorType read FVendor write FVendor;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PFTPFileProperty read Get; default;
    property Names: TPointerHashTable read FNames;
  end;
  //ftp
  TgFTP = class(TgSocket)
  protected
    FParseList: Boolean;
    FUserId: String;
    FPassword: String;
    FCurrentDir: String;
    FDirList: TFTPDirectoryList;
    //misc
    FDataSocket: TSocket;
    FDataPort: Word;
    FBusy: Boolean;
    FModeType: TModeType;
    FEnabledRestore: Boolean;
    FEndOfDataSocket: Boolean;
    FPassiveMode: Boolean;

    //event
    FOnAuthenticationFailed: THandlerEvent;
    FOnAuthenticationNeeded: THandlerEvent;
    FOnTransActionStart: TNotifyEvent;
    FOnTransActionStop: TNotifyEvent;
    FOnSuccess: TSuccessEvent;
    FOnUnSupportFunction: TSuccessEvent;
    FOnListItem: TListItemEvent;
    FOnFailure: TFailureEvent;

    procedure SetVendor(const Value: TVendorType);
    function GetVendor: TVendorType;
    procedure Response;
    function GetDataSocket(Passive: Boolean): TSocket;
    procedure GetFile(const RemoteFile,LocalFile: String; Passive: Boolean;
      MT: TModeType; DoRestore: Boolean); virtual;
    procedure PutFile(const RemoteFile: String; Stream: TStream; Passive: Boolean;
      MT: TModeType; DoAppend,DoUniqueUpload: Boolean; Position: Integer);
    function EOS(var Socket:TSocket): Boolean; override;
    procedure SetEndOfSocket(var Socket: TSocket; Value: Boolean); override;

  public
    constructor Create(BufferSize: Integer = BUFFER_SIZE); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    procedure Abort; override;
    procedure Allocate(FileSize: Integer);
    procedure ChangeDir(const DirName: String);
    procedure Delete(const FileName: String);
    procedure DoCommand(const CommandStr: String; Dummy: Boolean = False); override;
    procedure Download(const RemoteFile,LocalFile: String); virtual;
    procedure DownloadRestore(const RemoteFile,LocalFile: String);
    function GetLocalPortString: String; override;
    procedure List;
    procedure MakeDir(const DirName: String);
    procedure Mode(TheMode: TModeType);
    procedure Nlist;
    procedure Reinitialize;
    procedure RemoveDir(const DirName: String);
    procedure Rename(const FileName,FileName2: String);
    procedure Upload(const LocalFile,RemoteFile: String); virtual;
    procedure UploadAppend(const LocalFile,RemoteFile: String);
    procedure UploadRestore(const LocalFile,RemoteFile: String; Position: Integer); virtual;
    procedure UploadUnique(const LocalFile: String);
    function GetSimpleProperty(const RemoteFile: String): TFtpFileProperty; virtual;
    function PreDownload(const RemoteFile: String; Stream: TStream;
      Passive: Boolean; MT: TModeType): Integer; overload;
    function PreDownload(const RemoteFile: String; BeginPosition: Integer;
      Passive: Boolean; MT: TModeType): Integer; overload;
    procedure FinishDownload;
    procedure GetList(const CommandStr: String; List: TFTPDirectoryList; Passive: Boolean); virtual;
    function ReadVar(var Buf; Size: Integer): Integer; override;
    procedure PrintWorkDir;
    procedure FindFiles(Directory,WildCard: String;
      Files: TStrings; Recurce: Boolean = False);

    property CurrentDir: String read FCurrentDir;
    property FTPDirectoryList: TFTPDirectoryList read FDirList write FDirList;
    property ParseList: Boolean read FParseList write FParseList;
    property Password: String read FPassword write FPassword;
    property UserId: String read FUserId write FUserId;
    property Vendor: TVendorType read GetVendor write SetVendor;
    property PassiveMode: Boolean read FPassiveMode write FPassiveMode;
    property EnabledRestore: Boolean read FEnabledRestore;
    //event
    property OnAuthenticationFailed: THandlerEvent read FOnAuthenticationFailed write FOnAuthenticationFailed;
    property OnAuthenticationNeeded: THandlerEvent read FOnAuthenticationNeeded write FOnAuthenticationNeeded;
    property OnFailure: TFailureEvent read FOnFailure write FOnFailure;
    property OnSuccess: TSuccessEvent read FOnSuccess write FOnSuccess;
    property OnTransActionStart: TNotifyEvent read FOnTransActionStart write FOnTransActionStart;
    property OnTransActionStop: TNotifyEvent read FOnTransActionStop write FOnTransActionStop;
    property OnUnSupportFunction: TSuccessEvent read FOnUnSupportFunction write FOnUnSupportFunction;
    property OnListItem: TListItemEvent read FOnListItem write FOnListItem;
  end;

  

implementation

procedure ClearFtpFileProperty(var Item: TFtpFileProperty);
//プロパティをクリアする
begin
  Item.Size := 0;
  Item.Name := '';
  Item.ModifiDate := Now;
  Item.FileType := ftNone;
  Item.Attribute.Group := EmptyUnixFileMode;
  Item.Attribute.Other := EmptyUnixFileMode;
  Item.Attribute.User := EmptyUnixFileMode;
end;

function GetFtpFileAttr(var Item: TFtpFileProperty): Integer;
//ファイル属性の値を返す
begin
  Result := 0;
  with Item.Attribute do
  begin
    if User.Read then
      Inc(Result,400);

    if User.Write then
      Inc(Result,200);

    if User.Execute then
      Inc(Result,100);

    if Group.Read then
      Inc(Result,40);

    if Group.Write then
      Inc(Result,20);

    if Group.Execute then
      Inc(Result,10);

    if Other.Read then
      Inc(Result,4);

    if Other.Write then
      Inc(Result,2);

    if Other.Execute then
      Inc(Result,1);
  end;
end;

function ParseLineSimple(Line: String; var Item: TFTPFileProperty): Boolean;
//シンプル解析
var
  sl: TStringList;
begin
  Result := False;
  ClearFtpFileProperty(Item);
  if Line = '' then
    Exit;

  sl := TStringList.Create;
  try
    try
      SplitRegExpr('\s+',Line,sl);
      if sl.Count <> 1 then
        Exit
      else begin
        Item.FileType := ftFile;
        Item.Name := sl[0];
        Result := True;
      end;

    except
    end;
  finally
    sl.Free;
  end;
  
end;

function ParseLineUnix(Line: String; var Item: TFTPFileProperty): Boolean;
//ディレクトリー解析
//dirname:   ..カレントでは存在しない
//total 123
//-rw-rw-r--   1 16354    20         19307 Sep  7 16:33 09n7321.txt
// [0]        [1][2]     [3]         [4]   [5] [6][7]   [8]
//
//-rwxrwxrwx  1 ftp      ftp           344 Jul  1  1999 httproot.txt
//0           1  2         3            4   5   6   7     8
//
// -rwxrw-r--   1 22       6724870 Apr  7 10:30 mozilla-win32.zip
// [0]         [1][2]       [3]    [4] [5] [6]  [7]
var
  Year,Mon,Day,Hour,Min,Sec: Word;
  sl: TStringList;
  T: Char;
begin
  Result := False;
  ClearFtpFileProperty(Item);
  if Line = '' then
    Exit;

  T := LowerCase(Line)[1];

  sl := TStringList.Create;
  try
    try
      case T of
        'd': Item.FileType := ftDir;
        '-': Item.FileType := ftFile;
        'l': Item.FileType := ftLink;
      else
        //違っていたら終り
        Exit;
      end;
      //OK
      Result := True;

      SplitRegExpr('\s+',Line,sl);
      //file name
      Item.Name := sl[sl.Count - 1];
      //size //後ろから数える
      try
        Item.Size := StrToInt(sl[4])
      except
        on EConvertError do
          Item.Size := StrToIntDef(sl[3],0);
      end;

      DecodeDate(Now,Year,Mon,Day);
      //月
      Mon := GetMonth(sl[sl.Count - 4]);
      //日
      Day := StrToIntDef(sl[sl.Count - 3],Day);
      //時間
      Hour := 0;
      Min := 0;
      Sec := 0;
      if Pos(':',sl[sl.Count - 2]) > 0 then
      begin
        Hour := StrToIntDef(Copy(sl[sl.Count - 2],1,2),0);
        Min := StrToIntDef(Copy(sl[sl.Count - 2],4,2),0);
      end
      //年
      else
        Year := StrToIntDef(sl[sl.Count - 2],0);

      Item.ModifiDate := EncodeDate(Year,Mon,Day) + EncodeTime(Hour,Min,Sec,0);
      //日付を比較して 大きければ一年前にする
      if Now < Item.ModifiDate then
        Item.ModifiDate := IncMonth(Item.ModifiDate,-12);
    except
      Result := False;
    end;
  finally
    sl.Free;
  end;
end;

function ParseLineDos(Line: String; var Item: TFTPFileProperty): Boolean;
//dos解析
var
  SD,ST: String;
  DS: Char;
  sl: TStringList;
begin
  Result := False;
  ClearFtpFileProperty(Item);
  if Line = '' then
    Exit;

  DS := DateSeparator;
  SD := ShortDateFormat;
  ST := ShortTimeFormat;

  sl := TStringList.Create;
  try
    try
      SplitRegExpr('\s+',Line,sl);

      if Pos('<DIR>',Line) > 0 then
        Item.FileType := ftDir
      else
        Item.FileType := ftFile;

      DateSeparator:='-';
      ShortDateFormat:='mm/dd/yy';
      Shorttimeformat:='hh:nnAM/PM';
      try
        Item.Size := StrToInt(RemoveComma(sl[1]));
        //拡張子なし
        Item.Name := sl[0];
        Item.ModifiDate := StrToDateTime(sl[2] + ' ' + sl[3]);
      except
        //拡張子あり
        try
          Item.Size := StrToInt(RemoveComma(sl[2]));
          Item.Name := sl[0] + '.' + sl[1];
          Item.ModifiDate := StrToDateTimeDef(sl[3] + ' ' + sl[4],Now);
        except
        end;
      end;
      //日付を比較して 大きければ一年前にする
      if Now < Item.ModifiDate then
        Item.ModifiDate := IncMonth(Item.ModifiDate,-12);

      Result := True;
    except
      Result := False;
    end;
  finally
    DateSeparator := DS;
    ShortdateFormat := SD;
    Shorttimeformat := ST;

    sl.Free;
  end;
end;

function ParseFTPLine(Line: String; var Item: TFTPFileProperty): Boolean;
//FTP List Line自動解析
var
  L: String;
  sl: TStringList;
begin
  Result := False;
  ClearFtpFileProperty(Item);
  if Line = '' then
    Exit;

  L := LowerCase(Line);
  sl := TStringList.Create;
  try
    SplitRegExpr('\s+',Line,sl);

    if (Copy(L,1,5)) = 'total' then
      Result := False
    else if sl.Count = 1 then
      Result := ParseLineSimple(Line,Item)
    else if L[1] in ['d','l','-','s'] then
      Result := ParseLineUnix(Line,Item)
    else if L[1] in ['0'..'9'] then
      Result := ParseLineDos(Line,Item);
  finally
    sl.Free;
  end;
end;


{ TFTPDirectoryList }

function TFTPDirectoryList.Add(var Item: TFtpFileProperty): Integer;
//加える
var
  p: PFtpFileProperty;
begin
  New(p);
  p^ := Item;

  Result := FItems.Add(p);
  FNames[p^.Name] := p;
end;

procedure TFTPDirectoryList.Assign(Source: TFTPDirectoryList);
// self <= Source へコピー
var
  i: Integer;
  Item: TFTPFileProperty;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    Item := Source[i]^;
    Add(Item);
  end;
end;

procedure TFTPDirectoryList.Clear;
//clear
var
  i: Integer;
begin
  for i := FItems.Count - 1 downto 0 do
    Delete(i);

  FItems.Clear;
  FNames.Clear;
end;

constructor TFTPDirectoryList.Create;
begin
  inherited Create;
  FItems := TListPlus.Create;
  FNames := TPointerHashTable.Create(HASH_50,False);
  FNames.RaiseException := False;
  FVendor := vtAuto;
end;

procedure TFTPDirectoryList.Delete(Index: Integer);
//削除する
var
  p: PFtpFileProperty;
begin
  p := FItems[Index];
  FNames.Remove(p^.Name);
  FItems.Delete(Index);

  Dispose(p);
end;

destructor TFTPDirectoryList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FNames);
  inherited Destroy;
end;

function TFTPDirectoryList.Get(Index: Integer): PFTPFileProperty;
//itemをget
begin
  Result := FItems[Index];
end;

function TFTPDirectoryList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFTPDirectoryList.NameExists(FileName: String): Boolean;
//ファイル名が存在するか？
begin
  Result := FNames.HasKey(Filename);
end;

procedure TFTPDirectoryList.ParseLine(Line: String);
//ftp directory line 解析
var
  item: TFTPFileProperty;
  ok: Boolean;
begin
  item.Line := Line;
  if FVendor = vtUnix then
    ok := ParseLineUnix(Line,item)
  else if FVendor = vtDos then
    ok := ParseLineDos(Line,item)
  else
    ok := ParseFTPLine(Line,item);

  if ok then
    Add(item)
end;


{ TgFTP }

procedure TgFTP.Abort;
//ABOR
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Abort',Status_Trace);
  Report('nfo>abortを開始します',Status_Informational);
  Handler := False;
  Suc := False;
  try
    DoCommand('ABOR');
    Response;
  except
    on EProtocolError do
    begin
      Response;
      FBusy := False;
      Suc := True;
    end
    else begin
      //失敗
      Suc := False;
      FBusy := False;
      Report('err>ABORコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdAbort);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>abort終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdAbort);
  end;
end;

procedure TgFTP.Allocate(FileSize: Integer);
//ALLO
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Allocate',Status_Trace);
  Report('nfo>allocateを開始します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('ALLO ' + IntToStr(FileSize));
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>ALLOコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdAllocate);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdAllocate);
      //false だと再生成
      if not Handler then raise;
    end;
    else begin
      //失敗
      Suc := False;
      Report('err>ALLOコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdAllocate);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>allocate終了しました',Status_Informational);
    if  Assigned(FOnSuccess) then FOnSuccess(cmdAllocate);
  end;
end;

procedure TgFTP.ChangeDir(const DirName: String);
//dir変更
var
  Handler,Suc: Boolean;
  S: String;
begin
  if DirName = '' then
    Exit;

  Report('nfo>currentdirを' + DirName + 'に変更します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('CWD ' + DirName);
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>CWDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdChangeDir);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdChangeDir);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>CWDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then
        FOnFailure(Handler,cmdChangeDir);
      //false だと再生成
      if not Handler then
        raise;
    end;
  end;
  //成功
  if Suc then
  begin
    //current dir 変更
    S := Copy(FStatus,Pos('"',FStatus) + 1,Length(FStatus));
    FCurrentDir := Copy(S,1,Pos('"',S) - 1);
    //direcory listをclear
    FDirList.Clear;
    Report('nfo>change dir終了しました',Status_Informational);
    if Assigned(FOnSuccess) then
      FOnSuccess(cmdChangeDir);
  end;
end;

procedure TgFTP.Connect;
var
  Needed,Failed: Boolean;
  Cnt,Index: Integer;
  TempHost: String;
  TempPort: Word;
begin
//  Report('trc>TgFTP.Connect',Status_Trace);

  Cnt := 0;
  //接続中にconnectを呼んでも再接続しない
  if not FConnected then
  begin
    //proxyがあれば
    if FProxy <> '' then
    begin
      Index := Pos(':',FProxy);
      if Index > 0 then
      begin
        TempHost := Trim(Copy(FProxy,1,Index - 1));
        TempPort := StrToIntDef(Trim(Copy(FProxy,Index + 1,MaxInt)),21);
        ConnectHost(TempHost,TempPort);
      end
      else
        ConnectHost(FProxy,21);
    end
    else
      inherited Connect;

    Response;
  end;

  FModeType := MODE_UNKNOWN;
  FBusy := False;
  FEnabledRestore := False;
  CloseSocket(FDataSocket);
  FDataSocket := INVALID_SOCKET;
  FDataPort := 20;
  //direcory listをclear
  FDirList.Clear;

  Report('nfo>loginします',Status_Informational);
  while Cnt < 2 do
  begin
    Needed := False;
    Failed := False;
    //カウント 2になったら終り
    Inc(Cnt);
    //needed
    if (FUserId = '') then
    begin
      if Assigned(FOnAuthenticationNeeded) then
        FOnAuthenticationNeeded(Needed);

      if Needed and (Cnt < 2) then
        Continue
      else begin
        Report('err>useridがありません',Status_Basic);
        raise EProtocolError.Create('AuthenticationNeeded',FStatus,FStatusNo);
      end;
    end;
    
    try
      //サーバの準備完了ならば
      if FStatusNo = 220 then
      begin
        //proxyがあれば  USER@HOST
        if FProxy <> '' then
          DoCommand('USER '+ FUserId + '@' + FHost)
        else
          DoCommand('USER '+ FUserId);

        Response;
        //パスが必要ならば
        if FStatusNo = 331 then
        begin
          DoCommand('PASS '+ FPassword);
          Response;
        end;
        //ログイン完了ならば抜ける
        if FStatusNo = 230 then
          Break
        else begin
          Report('err>login失敗しました',Status_Basic);
          raise EProtocolError.Create('ftp login failed',FStatus,FStatusNo);
        end;
      end;
    except
      if Assigned(FOnAuthenticationFailed) then
        FOnAuthenticationFailed(Failed);

      if Failed and (Cnt < 2) then
        Continue
      else 
        raise
    end;
  end;  

end;

constructor TgFTP.Create(BufferSize: Integer);
begin
  inherited Create(BufferSize);
  FParseList := True;
  FUserId := 'anonymous';
  FPort := 21;
  //FProxyPort := 21;
  FDataSocket := INVALID_SOCKET;
  FDataPort := 20;
  FPassiveMode := False;
  FDirList := TFTPDirectoryList.Create;
end;

procedure TgFTP.Delete(const FileName: String);
//DELE
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Delete',Status_Trace);
  Report('nfo>' + FileName + 'を削除します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('DELE ' + FileName);
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>DELEコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdDelete);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDelete);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>DELEコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDelete);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>delete終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdDelete);
  end;
end;

destructor TgFTP.Destroy;
begin
  FreeAndNil(FDirList);
  inherited Destroy;
end;

procedure TgFTP.Disconnect;
//ftp 切断
begin
//  Report('trc>TgFTP.Disconnect',Status_Trace);

  try
    if FBusy then Abort;
  except
    ;
  end;

  if FConnected then
  begin
    Report('nfo>logoutします',Status_Informational);
    try
      DoCommand('QUIT');
      Response;
    except
      ;
    end;
  end;

  CloseSocket(FDataSocket);
  FBusy := False;
  inherited Disconnect;
end;

procedure TgFTP.DoCommand(const CommandStr: String; Dummy: Boolean);
// send command
begin
  if Pos('PASS',CommandStr) = 1 then
  begin
    WriteLn(CommandStr);
    Report('cmd>PASS ' + StringOfChar('*',Length(CommandStr) - 5),Status_Basic);
  end
  else
    inherited;
end;

procedure TgFTP.Download(const RemoteFile, LocalFile: String);
//新規download
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Download',Status_Trace);
  Report('nfo>' + RemoteFile + 'のdownloadを開始します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    //ファイルを最初から取ってくる
    GetFile(RemoteFile,LocalFile,FPassiveMode,FModeType,False);
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>download失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdDownload);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDownload);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>download失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDownload);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>download終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdDownload);
  end;
end;

procedure TgFTP.DownloadRestore(const RemoteFile, LocalFile: String);
//resume download
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.DownloadRestore',Status_Trace);
  Report('nfo>' + RemoteFile + 'のdownloadを開始します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    //fileの最後に追記
    GetFile(RemoteFile,LocalFile,FPassiveMode,FModeType,True);
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>downloadを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdDownRestore);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDownRestore);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>downloadを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdDownRestore);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>download終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdDownRestore);
  end;
end;

function TgFTP.GetDataSocket(Passive: Boolean): TSocket;
//データ socketを返す
var
  S,IP: String;
  SL: TStringList;
begin
  Result := INVALID_SOCKET;
  //PORTの場合実際に返すのは Listen用のSocketである
  //PORTは
  //bind - listen - PORT - (REST) - RETR - accept の順番で行う
  if not Passive then
  begin
    //Listen socketを作成
    Result := CreateSocket;
    //Bind
    BindSocket(Result,10000,50000,FDataPort);
    DoCommand('PORT ' + GetLocalAddress + GetLocalPortString);
    Response;
    //例外
    if not((FStatusNo = 200) or (FStatusNo = 250)) then
    begin
      Report('err>PORTコマンドを失敗しました',Status_Basic);
      raise EProtocolError.Create('ftp port',FStatus,FStatusNo);
    end;

  end
  else begin
  //PASVの場合は実際のDATA Socketを返す
    DoCommand('PASV');
    Response;
    //例外
    if FStatusNo <> 227 then
    begin
      Report('err>PASVコマンドを失敗しました',Status_Basic);
      raise EProtocolError.Create('ftp pasv',FStatus,FStatusNo);
    end
    else begin
      //227 Entering Passive Mode (192,168,0,1,4,76)
      S := Copy(FStatus,Pos('(',FStatus) + 1,Length(FStatus));
      S := Copy(S,1,Pos(')',S) - 1);
      SL := TStringList.Create;
      try
        SplitRegExpr(',',S,SL);
        IP := SL[0] + '.' + SL[1] + '.' + SL[2] + '.' + SL[3];
        FDataPort := StrToInt(SL[4]) * 256 + StrToInt(SL[5]);
      finally
        SL.Free;
      end;

      FIPAddress := inet_addr(PChar(IP));
      //サーバとConnect
      ConnectSocket(Result,FDataPort,FIPAddress);
      if Result = INVALID_SOCKET then
      begin
        Report('err>data socketの作成に失敗しました',Status_Basic);
        raise ESocketError.Create(WSAGetLastError);
      end;
    end;
  end;

end;

procedure TgFTP.GetFile(const RemoteFile,LocalFile: String; Passive: Boolean;
  MT: TModeType; DoRestore: Boolean);
//TYPE 
//PORT or PASV
//REST
//accept  ----
//RETR
var
  Socket: TSocket;
  FS: TFileStream;
begin
  CloseSocket(FDataSocket);
  //存在しなければ作成
  if not FileExists(LocalFile) then
    FS := TFileStream.Create(LocalFile,fmCreate or fmShareDenyWrite)
  else
    FS := TFileStream.Create(LocalFile,fmOpenWrite or fmShareDenyWrite);

  try
    //resumeするか？
    if DoRestore then
      FS.Seek(0,soFromEnd)
    else begin
      FS.Seek(0,soFromBeginning);
      FS.Size := 0;
    end;

    if FModeType <> MT then
      Mode(MT);
    //PORT or PASV
    if Passive then
      FDataSocket := GetDataSocket(Passive)
    else 
      Socket := GetDataSocket(Passive);

    //restore check
    FEnabledRestore := True;
    try
      DoCommand('REST ' + IntToStr(FS.Position));
      Response;
    except
      on EProtocolError do
      begin
        //restoreできないので最初から
        FEnabledRestore := False;
        FS.Seek(0,soFromBeginning);
        FS.Size := 0;
      end
      else begin
        FEnabledRestore := False;
        //再生成
        raise;
      end;
    end;

    //PORT or PASV
    if Passive then
    begin
      DoCommand('RETR '+ RemoteFile);
      Response;
    end
    else begin
      DoCommand('RETR '+ RemoteFile);
      Response;
      FDataSocket := AcceptSocket(Socket);
      CloseSocket(Socket);
      if FDataSocket = INVALID_SOCKET then
        raise ESocketError.Create(WSAGetLastError);
    end;
    //data socket 開始
    FBusy := True;
    if Assigned(FOnTransActionStart) then
      FOnTransActionStart(Self);
    
    //streamに読み込み
    CaptureFromSocket(FDataSocket,FS,-1,FS.Position);
    if Assigned(FOnTransActionStop) then
      FOnTransActionStop(Self);
  finally
    FS.Free;
    FBusy := False;
    CloseSocket(FDataSocket);
  end;
  //忘れずに
  Response;
end;

procedure TgFTP.GetList(const CommandStr: String; List: TFTPDirectoryList; Passive: Boolean);
//list受信
var
  Socket: TSocket;
  Line: String;
begin
  CloseSocket(FDataSocket);
  try
    if FModeType <> MODE_ASCII then
      Mode(MODE_ASCII);
    //PORT or PASV
    if Passive then
    begin
      FDataSocket := GetDataSocket(Passive);
      DoCommand(CommandStr);
      Response;
    end
    else begin
      Socket := GetDataSocket(Passive);
      DoCommand(CommandStr);
      Response;
      FDataSocket := AcceptSocket(Socket);
      CloseSocket(Socket);
      if FDataSocket = INVALID_SOCKET then
        raise ESocketError.Create(WSAGetLastError);
    end;

    FBusy := True;
    if Assigned(FOnTransActionStart) then
      FOnTransActionStart(Self);

    //directory listを読む
    List.Clear;
    repeat
      Line := ReadLnFromSocket(FDataSocket);
      //解析
      if FParseList then
        List.ParseLine(Line)
      else begin
        //解析しないときはイベント
        if Assigned(FOnListItem) then
          FOnListItem(Line);
      end;

    until EOS(FDataSocket);

    if Assigned(FOnTransActionStop) then
      FOnTransActionStop(Self);
  finally
    FBusy := False;
    CloseSocket(FDataSocket);
  end;
  //忘れずに
  Response;
end;

function TgFTP.GetLocalPortString: String;
//ftpで使用する portを返す
begin
//  Report('trc>TgFTP.GetLocalPortString',Status_Trace);

  Result := IntToStr(FDataPort and $ff00 shr 8)+','+
            IntToStr(FDataPort and $00ff);
end;

function TgFTP.GetVendor: TVendorType;
begin
//  Report('trc>TgFTP.GetVendor',Status_Trace);

  Result := FDirList.Vendor;
end;

procedure TgFTP.List;
//LIST
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.List',Status_Trace);
  Report('nfo>listを取得します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    GetList('LIST',FDirList,FPassiveMode);
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>LISTコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdList);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdList);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>LISTコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdList);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>list取得終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdList);
  end;
end;

procedure TgFTP.MakeDir(const DirName: String);
//MKD
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.MakeDirectory',Status_Trace);
  Report('nfo>' + DirName + 'を作成します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('MKD ' + DirName);
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>MKDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdMakeDir);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdMakeDir);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>MKDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdMakeDir);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>make directory終了しました',Status_Informational);
    if  Assigned(FOnSuccess) then FOnSuccess(cmdMakeDir);
  end;
end;

procedure TgFTP.Mode(TheMode: TModeType);
//TYPE
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Mode',Status_Trace);
  Report('nfo>file modeを変更します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    if TheMode = MODE_ASCII then
      DoCommand('TYPE A')
    else if TheMode = MODE_BYTE then
      DoCommand('TYPE B')
    else
      DoCommand('TYPE I');

    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>TYPEコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdMode);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdMode);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>TYPEコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdMode);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc and Assigned(FOnSuccess) then
  begin
    FModeType := TheMode;
    Report('nfo>file mode終了しました',Status_Informational);
    FOnSuccess(cmdMode);
  end;
  
end;

procedure TgFTP.Nlist;
//NLST
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Nlist',Status_Trace);
  Report('nfo>listを取得します',Status_Informational);
  
  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    GetList('NLST',FDirList,FPassiveMode);
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>NLSTコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdNlist);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdNlist);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>NLSTコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdNlist);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>list取得終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdNlist);
  end;
end;

procedure TgFTP.PrintWorkDir;
//dir確認
var
  Handler,Suc: Boolean;
  S: String;
begin
//  Report('trc>TgFTP.PrintWorkDir',Status_Trace);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('PWD');
    Response;
  except
    //失敗
    Suc := False;
    Report('err>PWDコマンドを失敗しました',Status_Basic);
    if Assigned(FOnFailure) then
      FOnFailure(Handler,cmdCurrentDir);
    //false だと再生成
    if not Handler then raise;
  end;
  //成功
  if Suc then
  begin
    //current dir 変更
    S := Copy(FStatus,Pos('"',FStatus) + 1,Length(FStatus));
    FCurrentDir := Copy(S,1,Pos('"',S) - 1);
    if Assigned(FOnSuccess) then
      FOnSuccess(cmdCurrentDir);
  end;
end;

procedure TgFTP.Reinitialize;
//REIN
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Reinitialize',Status_Trace);
  Report('nfo>接続を初期化します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('REIN');
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>REINコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdReinit);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdReinit);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>REINコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdReinit);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>reinitialize終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdReinit);
  end;
end;

procedure TgFTP.RemoveDir(const DirName: String);
//RMD
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.RemoveDir',Status_Trace);
  Report('nfo>' + DirName + 'を削除します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('RMD ' + DirName);
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>RMDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdRemoveDir);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdRemoveDir);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>RMDコマンドを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdRemoveDir);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>remove directory終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdRemovedir);
  end;
end;

procedure TgFTP.Rename(const FileName, FileName2: String);
var
  Handler,Suc: Boolean;
begin
//  Report('trc>TgFTP.Rename',Status_Trace);
  Report('nfo>' + FileName + 'を' + FileName2 + 'に変更します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    DoCommand('RNFR ' + FileName);
    Response;
    DoCommand('RNTO ' + FileName2);
    Response;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>renameを失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdRename);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdRename);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>renameを失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdRename);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>rename終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdRename);
  end;
end;

procedure TgFTP.Response;
//FTPレスポンスコード
var
  S: String;
begin
//  Report('trc>TgFTP.Response',Status_Trace);

  FStatusNo := 0;
  repeat
    S := ResultCommand;
    try
      //数値化できればおわり
      //200    終り
      //200-   終らない
      StrToInt(Trim(Copy(S,1,4)));
      Break;
    except
      ;
    end;
  until EOS(FSocket) ;

  FStatusNo := StrToIntDef(Copy(S,1,3),999);
  FStatus := Copy(S,5,Length(S));
  if FStatusNo >= 400 then
  begin
    Report('err>' + IntToStr(FStatusNo) + ' ' + FStatus,Status_Basic);
    raise EProtocolError.Create('ftp',FStatus,FStatusNo);
  end;
end;


procedure TgFTP.SetVendor(const Value: TVendorType);
begin
//  Report('trc>TgFTP.SetVendor',Status_Trace);

  FDirList.Vendor := Value;
end;

procedure TgFTP.Upload(const LocalFile, RemoteFile: String);
//STOR
var
  Handler,Suc: Boolean;
  FS: TFileStream;
begin
//  Report('trc>TgFTP.Upload',Status_Trace);
  Report('nfo>' + LocalFile + 'のuploadを開始します',Status_Informational);

  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    if not FileExists(LocalFile) then
      raise ELocalError.Create('upload file not found');

    FS := TFileStream.Create(LocalFile,fmOpenRead or fmShareDenyWrite);
    try
      //ファイルを最初から送る
      PutFile(RemoteFile,FS,FPassiveMode,FModeType,False,False,0);
    finally
      FS.Free;
    end;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdUpload);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpload);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpload);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>upload終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdUpload);
  end;
end;

procedure TgFTP.UploadAppend(const LocalFile, RemoteFile: String);
//APPE
var
  Handler,Suc: Boolean;
  FS: TFileStream;
begin
//  Report('trc>TgFTP.UploadAppend',Status_Trace);
  Report('nfo>' + LocalFile + 'のuploadを開始します',Status_Informational);
  
  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    if not FileExists(LocalFile) then
      raise ELocalError.Create('upload file not found');

    FS := TFileStream.Create(LocalFile,fmOpenRead or fmShareDenyWrite);
    try
      //ファイルを追加で送る
      PutFile(RemoteFile,FS,FPassiveMode,FModeType,True,False,0);
    finally
      FS.Free;
    end;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdAppend);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdAppend);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdAppend);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>upload終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdAppend);
  end;
end;

procedure TgFTP.UploadRestore(const LocalFile, RemoteFile: String;
  Position: Integer);
//APPE2
var
  Handler,Suc: Boolean;
  FS: TFileStream;
begin
//  Report('trc>TgFTP.UploadRestore',Status_Trace);
  Report('nfo>' + LocalFile + 'のuploadを開始します',Status_Informational);
  
  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    if not FileExists(LocalFile) then
      raise ELocalError.Create('upload file not found');

    FS := TFileStream.Create(LocalFile,fmOpenRead or fmShareDenyWrite);
    try
      //ファイルを最初から送る
      PutFile(RemoteFile,FS,FPassiveMode,FModeType,True,False,Position);
    finally
      FS.Free;
    end;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdUpRestore);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpRestore);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpRestore);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>upload終了しました',Status_Informational);
    if  Assigned(FOnSuccess) then FOnSuccess(cmdUpRestore);
  end;
end;

procedure TgFTP.UploadUnique(const LocalFile: String);
//STOR 同じファイル名が存在していれば違うファイル名にしてアップ
var
  Handler,Suc: Boolean;
  FS: TFileStream;
  RemoteFile: String;
  List: TFTPDirectoryList;
  Event: TListItemEvent;
  aParseList: Boolean;
begin
//  Report('trc>TgFTP.UploadUnique',Status_Trace);
  Report('nfo>' + LocalFile + 'のuploadを開始します',Status_Informational);

  RemoteFile := ExtractFileName(LocalFile);
  Handler := False;
  Suc := True;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    if not FileExists(LocalFile) then
      raise ELocalError.Create('upload file not found');

    FS := TFileStream.Create(LocalFile,fmOpenRead or fmShareDenyWrite);
    List := TFTPDirectoryList.Create;
    //listitem eventを無効にしておく
    Event := FOnListItem;
    FOnListItem := nil;
    aParseList := FParseList;
    FParseList := True;

    try
      //同じファイル名チェック
      repeat
        GetList('LIST ' + RemoteFile,List,FPassiveMode);
        //filenameが無ければ終り
        if not List.NameExists(RemoteFile) then
          Break;

        RemoteFile := UniqueFileName(RemoteFile);
      until False ;
      //ファイルを最初から送る
      PutFile(RemoteFile,FS,FPassiveMode,FModeType,False,False,0);

    finally
      FOnListItem := Event;
      FParseList := aParseList;
      FS.Free;
      List.Free;
    end;
  except
    on EProtocolError do
    begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnUnSupportFunction) then FOnUnSupportFunction(cmdUpload);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpload);
      //false だと再生成
      if not Handler then raise;
    end
    else begin
      //失敗
      Suc := False;
      Report('err>upload失敗しました',Status_Basic);
      if Assigned(FOnFailure) then FOnFailure(Handler,cmdUpload);
      //false だと再生成
      if not Handler then raise;
    end;
  end;
  //成功
  if Suc then
  begin
    Report('nfo>upload終了しました',Status_Informational);
    if Assigned(FOnSuccess) then FOnSuccess(cmdUpload);
  end;
end;

procedure TgFTP.PutFile(const RemoteFile: String; Stream: TStream;  Passive: Boolean;
  MT: TModeType; DoAppend,DoUniqueUpload: Boolean; Position: Integer);
//TYPE
//PORT    or    PASV
//accept        ----
//STOR or APPE
var
  Socket: TSocket;
begin
  CloseSocket(FDataSocket);
  try
    Stream.Position := Position;
    if FModeType <> MT then
      Mode(MT);
    //PORT or PASV
    if Passive then
    begin
      FDataSocket := GetDataSocket(Passive);
      if DoAppend then
        DoCommand('APPE ' + RemoteFile)
      else if DoUniqueUpload then
        DoCommand('STOU ' + RemoteFile)
      else
        DoCommand('STOR ' + RemoteFile);

      Response;
    end
    else begin
      Socket := GetDataSocket(Passive);
      if DoAppend then
        DoCommand('APPE ' + RemoteFile)
      else if DoUniqueUpload then
        DoCommand('STOU ' + RemoteFile)
      else
        DoCommand('STOR ' + RemoteFile);

      Response;
      FDataSocket := AcceptSocket(Socket);
      CloseSocket(Socket);
      if FDataSocket = INVALID_SOCKET then
        raise ESocketError.Create(WSAGetLastError);
    end;
    //data socket 開始
    FBusy := True;
    if Assigned(FOnTransActionStart) then
      FOnTransActionStart(Self);

    //streamに書き込み
    SendToSocket(FDataSocket,Stream,-1,Position);
    if Assigned(FOnTransActionStop) then
      FOnTransActionStop(Self);
  finally
    FBusy := False;
    CloseSocket(FDataSocket);
  end;
  //忘れずに
  Response;
end;  

procedure TgFTP.SetEndOfSocket(var Socket: TSocket; Value: Boolean);
// eos set
begin
//  Report('trc>TgFTP.SetEndOfSocket',Status_Trace);

  if Socket = FSocket then
    FEOS := Value
  else if Socket = FDataSocket then
    FEndOfDataSocket := Value;
end;

function TgFTP.EOS(var Socket: TSocket): Boolean;
//end of socket check
begin
//  Report('trc>TgFTP.EOS',Status_Trace);

  if Socket = FDataSocket then
    Result := FEndOfDataSocket or (SocketState(FDataSocket) <> ssConnected)
  else if Socket = FSocket then
    Result := FEOS or (SocketState(FSocket) <> ssConnected)
  else
    Result := SocketState(Socket) <> ssConnected;
end;

function TgFTP.GetSimpleProperty(const RemoteFile: String): TFtpFileProperty;
//日付を取得するために LIST を最初に行う
var
  List: TFTPDirectoryList;
  Event: TListItemEvent;
  aParseList: Boolean;
  i: Integer;
begin
  ClearFtpFileProperty(Result);
  Result.Size := -1;
  try
    if FBusy then
      raise EProtocolBusy.Create('protocol busy');

    List := TFTPDirectoryList.Create;
    //listitem eventを無効にしておく
    Event := FOnListItem;
    FOnListItem := nil;
    aParseList := FParseList;
    FParseList := True;
    try
      GetList('LIST ' + RemoteFile,List,FPassiveMode);
      for i := 0 to List.Count - 1 do
        if (List[i].Name = RemoteFile) and (List[i].FileType = ftFile) then
        begin
          Result.Size := List[i].Size;
          Result.Name := List[i].Name;
          Result.Filetype := List[i].FileType;
          Result.ModifiDate := List[i].ModifiDate;
          Break;
        end;
    finally
      FOnListItem := Event;
      FParseList := aParseList;
      List.Free;
    end;

    //sizeがわからなければ SIZEコマンドを送る
    if Result.Size = -1 then
    begin
      //モード変更
      if FModeType <> MODE_IMAGE then
        Mode(MODE_IMAGE);

      DoCommand('SIZE ' + RemoteFile);
      Response;
      Result.Size := StrToInt(Trim(FStatus));
    end;
  except
  end;

  //0に戻す
  if Result.Size = -1 then
    Result.Size := 0;
end;



procedure TgFTP.FinishDownload;
begin
  if Assigned(FOnTransActionStop) then FOnTransActionStop(Self);

  FBusy := False;
  CloseSocket(FDataSocket);
  //忘れずに
  Response;
end;

function TgFTP.PreDownload(const RemoteFile: String; Stream: TStream;
  Passive: Boolean; MT: TModeType): Integer;
  //ダウンロード前処理
//TYPE
//PORT or PASV
//REST
//accept  ----
//RETR
begin
  Result := PreDownload(RemoteFile,Stream.Size,Passive,MT);
  if FEnabledRestore then
    Stream.Seek(0,soFromEnd)
  else begin
    //restoreできないので最初から
    Stream.Seek(0,soFromBeginning);
    Stream.Size := 0;
  end;
end;

function TgFTP.ReadVar(var Buf; Size: Integer): Integer;
begin
  Result := ReadBuffer(FDataSocket,Buf,Size);
end;

procedure TgFTP.FindFiles(Directory,WildCard: String;
  Files: TStrings; Recurce: Boolean);

  function CheckPath(S: String): String;
  begin
    Result := S;
    if (Result <> '') then
    begin
      if Result[Length(Result)] <> '/' then
        Result := Result + '/';
    end
    else
      Result := '/';
  end;

//ファイルを探す
var
  oldcurrent,path: String;
  dirlist: TFtpDirectoryList;
  dirnames: TStringList;
  i: Integer;
  p: PFtpFileProperty;
  isparse: Boolean;
begin
  oldcurrent := FCurrentDir;
  isparse := FParseList;
  dirlist := TFtpDirectoryList.Create;
  dirnames := TStringList.Create;
  try
    FParseList := True;
    //変更する
    if (Directory <> '') and
       (Directory <> FCurrentDir) then
      ChangeDir(Directory);

    GetList('LIST',dirlist,FPassiveMode);
    path := CheckPath(FCurrentDir);
    //解析
    for i := 0 to dirlist.Count - 1 do
    begin
      p := dirlist[i];
      if p^.FileType = ftDir then
        dirnames.Add(p^.Name)
      else if p^.FileType = ftFile then
      begin
        //指定なし
        if WildCard = '' then
          Files.Add(path + p^.Name)
        else if WildCardMatching(p^.Name,WildCard,True) then
          Files.Add(path + p^.Name);
      end;
    end;

    //再帰する
    if Recurce then
    begin
      for i := 0 to dirnames.Count - 1 do
        FindFiles(path + dirnames[i],WildCard,Files,Recurce);
    end;
  finally
    dirlist.Free;
    dirnames.Free;
    //ディレクトリを元に戻す
    if oldcurrent <> FCurrentDir then
      ChangeDir(oldcurrent);

    FParseList := isparse;
  end;
end;

function TgFTP.PreDownload(const RemoteFile: String;
  BeginPosition: Integer; Passive: Boolean; MT: TModeType): Integer;
  //ダウンロード前処理
//TYPE
//PORT or PASV
//REST
//accept  ----
//RETR
var
  Socket: TSocket;
  temp: String;
  re: TRegExpr;
begin
  //ファイルサイズを返す
  Result := -1;

  CloseSocket(FDataSocket);

  if FModeType <> MT then
    Mode(MT);
  //PORT or PASV
  if Passive then
    FDataSocket := GetDataSocket(Passive)
  else
    Socket := GetDataSocket(Passive);

  //restore check
  FEnabledRestore := True;
  try
    DoCommand('REST ' + IntToStr(BeginPosition));
    Response;
  except
    on EProtocolError do
    begin
      //sizeが0の時は不明
      if BeginPosition > 0 then
        FEnabledRestore := False
      else
        FEnabledRestore := True;
    end
    else begin
      FEnabledRestore := False;
      //再生成
      raise;
    end;
  end;

  //PORT or PASV
  if Passive then
  begin
    DoCommand('RETR '+ RemoteFile);
    Response;
  end
  else begin
    DoCommand('RETR '+ RemoteFile);
    Response;
    FDataSocket := AcceptSocket(Socket);
    CloseSocket(Socket);
    if FDataSocket = INVALID_SOCKET then
      raise ESocketError.Create(WSAGetLastError);
  end;

  re := TRegExpr.Create;
  try
    re.Expression := '.*\(([0-9,]+).*\)';
    if re.Exec(FStatus) then
    begin
      temp := RemoveComma(re.Match[1]);
      Result := StrToIntDef(temp,-1);
    end;
  finally
    re.Free;
  end;

  //data socket 開始
  FBusy := True;
  if Assigned(FOnTransActionStart) then
    FOnTransActionStart(Self);
end;

end.
