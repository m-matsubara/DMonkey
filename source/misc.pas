unit misc;

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  windows,sysutils,classes,shellapi;

//デフォルト付き日付
function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
function StrToTimeDef(const S: string; Default: TDateTime): TDateTime;

function StrToFloatDef(const S: string; F: Extended): Extended;

function GetFileSize(const FileName: String): Integer;

type
  TFindFileFlag = (fffNone,fffFile,fffDir,fffVolume,fffAny);

//ファイルを探す
procedure FindFiles(Folder,WildCard: String;
  Flag: TFindFileFlag; Files: TStrings; Folders: TStrings = nil);
//ファイルを削除する
procedure RemoveFiles(Handle: THandle; Files: TStrings;
  MoveTrash: Boolean ;Silent: Boolean = False);
//ファイルを移動
procedure MoveFiles(Handle: THandle; Files: TStrings; ToFolder: String);
//ファイルをコピー
procedure CopyFiles(Handle: THandle; Files: TStrings; ToFolder: String);
//フォルダをコピー
procedure CopyFolder(Handle: THandle; FromFolder,ToFolder: String);

//folderを列挙する
procedure EnumFolders(Folder: String; Folders: TStrings);
//folderをクリアする
procedure ClearFolder(Handle: THandle; Folder: String;
  MoveTrash,Delete: Boolean; Silent: Boolean = False);


implementation

function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
begin
  try
    Result := StrToDateTime(S);
  except
    Result := Default;
  end;
end;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
begin
  try
    Result := StrToDate(S);
  except
    on EConvertError do
      Result := Default;
  end;
end;

function StrToTimeDef(const S: string; Default: TDateTime): TDateTime;
begin
  try
    Result := StrToTime(S);
  except
    on EConvertError do
      Result := Default;
  end;
end;

function StrToFloatDef(const S: string; F: Extended): Extended;
begin
  try
    Result := StrToFloat(S);
  except
    on EConvertError do
      Result := F;
  end;
end;

function GetFileSize(const FileName: String): Integer;
//ファイルサイズを取得
var
  SR :TSearchRec;
begin
  //ファイルサイズ取得
  if FindFirst(FileName,faHidden or faArchive or faSysFile or faReadOnly ,SR) = 0 then
  begin
    Result := SR.Size;
    FindClose(SR);
  end
  else
    Result := 0;
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

//ファイルを探す
procedure FindFiles(Folder,WildCard: String; Flag: TFindFileFlag;
  Files: TStrings; Folders: TStrings = nil);

  function IsFile(Attr: Integer): Boolean;
  //ファイルかどうかチェック
  begin
    if (Attr and faVolumeID) > 0 then
      Result := False
    else if (Attr and faDirectory) > 0 then
      Result := False
    else
      Result := True;
  end;

  function IsFolder(Attr: Integer): Boolean;
  begin
    Result := (Attr and faDirectory) > 0;
  end;

var
  sr: TSearchRec;
  flags: Integer;
begin
  if not DirectoryExists(Folder) then
    Exit;

  //フラグ
  case Flag of
    fffNone: flags := 0;
    fffFile: flags := faReadOnly or faHidden or faSysFile or faArchive;
    fffDir: flags := faDirectory;
    fffVolume: flags := faVolumeID;
    fffAny: flags := faAnyFile;
  else
    flags := 0;
  end;

  Files.Clear;
  if Assigned(Folders) then
    Folders.Clear;

  Folder := IncludeTrailingBackSlash(Folder);
  if WildCard = '' then
    WildCard := '*.*';

  if FindFirst(Folder + WildCard,flags,sr) = 0 then
  begin
    if IsFile(sr.Attr) then
      Files.Add(Folder + sr.Name)
    else if Assigned(Folders) and IsFolder(sr.Attr) then
    begin
      if (sr.Name = '.') or (sr.Name = '..') then
      else
        Folders.Add(Folder + sr.Name);
    end;

    while FindNext(sr) = 0 do
      if IsFile(sr.Attr) then
        Files.Add(Folder + sr.Name)
      else if Assigned(Folders) and IsFolder(sr.Attr) then
      begin
        if (sr.Name = '.') or (sr.Name = '..') then
        else
          Folders.Add(Folder + sr.Name);
      end;

    FindClose(sr);
  end;
end;

//ファイルを削除する
procedure RemoveFiles(Handle: THandle; Files: TStrings; MoveTrash,Silent: Boolean);
//ファイル削除
var
  i: Integer;
  sfs: TSHFileOPStruct;
  deletefiles: String;
begin
  deletefiles := '';
  for i := 0 to Files.Count - 1 do
    if Files[i] <> '' then //ファイル名を絶対pathに変換する
      deletefiles := deletefiles + ExpandUNCFileName(Files[i]) + #0;

  if deletefiles <> '' then
  begin
    deletefiles := deletefiles + #0;

    FillChar(sfs,SizeOf(sfs),0);
    with sfs do
    begin
      Wnd := Handle;
      wFunc := FO_DELETE;
      pFrom := @deletefiles[1];
      pTo := nil;
      //flags
      if MoveTrash then
        fFlags := FOF_ALLOWUNDO
      else
        fFlags := 0;

      if Silent then
        fFlags := fFlags or FOF_SILENT or FOF_NOCONFIRMATION;
    end;
    //実行
    SHFileOperation(sfs);
  end;
end;

//ファイルを移動
procedure MoveFiles(Handle: THandle; Files: TStrings; ToFolder: String);
var
  i: Integer;
  sfs: TSHFileOPStruct;
  from,tempfolder: String;
begin
  from := '';
  ToFolder := IncludeTrailingBackSlash(ToFolder);
  //フォルダ作成
  if not DirectoryExists(ToFolder) then
    ForceDirectories(ToFolder);

  for i := 0 to Files.Count - 1 do
  begin
    if Files[i] <> '' then
    begin
      tempfolder := ExtractFilePath(Files[i]);
      //同じフォルダは無視する
      if AnsiCompareFilename(ToFolder,tempfolder) <> 0 then
        from := from + Files[i] + #0;
    end;
  end;

  if from <> '' then
  begin
    from := from + #0;
    ToFolder := ToFolder + #0;

    FillChar(sfs,SizeOf(sfs),0);
    With sfs do
    begin
      Wnd := Handle;
      wFunc := FO_MOVE;
      pFrom := @from[1];
      pTo := @ToFolder[1];
      fFlags := FOF_NOCONFIRMMKDIR;
    end;
    //実行
    SHFileOperation(sfs);
  end;
end;

//ファイルをコピー
procedure CopyFiles(Handle: THandle; Files: TStrings; ToFolder: String);
var
  i: Integer;
  sfs: TSHFileOPStruct;
  from,tempfolder: String;
begin
  from := '';
  ToFolder := IncludeTrailingBackSlash(ToFolder);
  //フォルダ作成
  if not DirectoryExists(ToFolder) then
    ForceDirectories(ToFolder);

  for i := 0 to Files.Count - 1 do
  begin
    if Files[i] <> '' then
    begin
      tempfolder := ExtractFilePath(Files[i]);
      //同じフォルダは無視する
      if AnsiCompareFilename(ToFolder,tempfolder) <> 0 then
        from := from + Files[i] + #0;
    end;
  end;

  if from <> '' then
  begin
    from := from + #0;
    ToFolder := ToFolder + #0;

    FillChar(sfs,SizeOf(sfs),0);
    With sfs do
    begin
      Wnd := Handle;
      wFunc := FO_COPY;
      pFrom := @from[1];
      pTo := @ToFolder[1];
      fFlags := FOF_NOCONFIRMMKDIR;
    end;
    //実行
    SHFileOperation(sfs);
  end;
end;

procedure CopyFolder(Handle: THandle; FromFolder,ToFolder: String);
//フォルダをコピー
var
  files,folders: TStringList;
begin
  files := TStringList.Create;
  folders := TStringList.Create;
  try
    FindFiles(IncludeTrailingBackslash(FromFolder),'*.*',fffany,files,folders);
    files.AddStrings(folders);
    CopyFiles(Handle,files,ToFolder);
  finally
    folders.Free;
    files.Free;
  end;
end;

procedure EnumFolders(Folder: String; Folders: TStrings);
//folderを列挙する
var
  files: TStringList;
begin
  files := TStringList.Create;
  try
    FindFiles(Folder,'*.*',fffDir,files,Folders);
  finally
    files.Free;
  end;
end;

procedure ClearFolder(Handle: THandle; Folder: String;
  MoveTrash,Delete,Silent: Boolean);
//folderをクリアする
var
  files,folders: TStringList;
begin
  if not DirectoryExists(Folder) then
    Exit;

  files := TStringList.Create;
  folders := TStringList.Create;
  try
    if Delete then
      files.Add(ExcludeTrailingBackslash(Folder))
    else begin
      FindFiles(Folder,'*.*',fffAny,files,folders);
      files.AddStrings(folders);
    end;

    if files.Count > 0 then
      RemoveFiles(Handle,files,MoveTrash,Silent);
  finally
    folders.Free;
    files.Free;
  end;
end;


end.
