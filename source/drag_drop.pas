unit drag_drop;

// 簡易Drag&Dropコンポーネント


interface

uses
  windows,sysutils,classes,activex,controls,comobj,shlobj;

type
  TDragDropFilesEvent = procedure(Sender: TWinControl; X,Y: Integer; Files: TStrings) of object;
  TDragDropTextEvent = procedure(Sender: TWinControl; X,Y: Integer; Text: String) of object;
  TDragDropOverEvent = procedure(Sender: TWinControl; X,Y: Integer; var Accept: Boolean) of object;

  TDragDropTarget = class(TComponent,IDropTarget)
  private
    FTarget: TWinControl;
    FTargetHandle: THandle;
    FEffect: Integer;
    FOnDragDropFiles: TDragDropFilesEvent;
    FOnDragDropText: TDragDropTextEvent;
    FOnDragDropOver: TDragDropOverEvent;

    procedure SetTarget(const Value: TWinControl);
  protected
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetFiles(HG: HGlobal; AFiles: TStrings);
    function GetText(HG: HGlobal): String;
  public
    destructor Destroy; override;
  published
    property Target: TWinControl read FTarget write SetTarget;
    property OnDragDropFiles: TDragDropFilesEvent read FOnDragDropFiles write FOnDragDropFiles;
    property OnDragDropText: TDragDropTextEvent read FOnDragDropText write FOnDragDropText;
    property OnDragDropOver: TDragDropOverEvent read FOnDragDropOver write FOnDragDropOver;
  end;

  TDragDropStr = String; //WideStringの方がいい？

  TDragDropTextDataObject = class(TInterfacedObject,IDataObject)
  private
    FText: TDragDropStr;
  protected
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult;
      stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
      stdcall;
  public
    constructor Create(AText: TDragDropStr);
  end;  

  TDragDropSource = class(TComponent,IDropSource)
  protected
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  published
    procedure DoDragFiles(AFolder: String; AFiles: TStrings);
    procedure DoDragText(AText: String);
  end;

  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDragDropTarget,TDragDropSource]);
end;

const
  HDropFormatEtc: TFormatEtc = (
    cfFormat: CF_HDROP;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: TYMED_HGLOBAL);

  TextFormatEtc: TFormatEtc = (
    cfFormat: CF_TEXT;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: TYMED_HGLOBAL);

{ TDragDropTarget }

destructor TDragDropTarget.Destroy;
begin
  SetTarget(nil);
  inherited;
end;

function TDragDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  if Succeeded(dataObj.QueryGetData(HDropFormatEtc)) then
    FEffect := DROPEFFECT_COPY
  else if Succeeded(dataObj.QueryGetData(TextFormatEtc)) then
    FEffect := DROPEFFECT_LINK
  else
    FEffect := DROPEFFECT_Link;

  dwEffect := FEffect;
  Result := S_OK;
end;

function TDragDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
end;

function TDragDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  accept: Boolean;
begin
  Result := S_OK;

  accept := True;
  if Assigned(FOnDragDropOver) then
    FOnDragDropOver(FTarget,pt.x,pt.y,accept);

  if accept then
    dwEffect := FEffect
  else
    dwEffect := DROPEFFECT_NONE;  
end;

function TDragDropTarget.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  medium : TStgMedium;
  sl: TStringList;
begin
  Result := S_OK;
  //ファイルのD&D
  if Succeeded(dataObj.GetData(HDropFormatEtc,medium)) then
  begin
    try
      if Assigned(FOnDragDropFiles) then
      begin
        sl := TStringList.Create;
        try
          GetFiles(medium.hGlobal,sl);
          //ファイルがあればイベント
          if sl.Count > 0 then
            FOnDragDropFiles(FTarget,pt.x,pt.y,sl);
        finally
          sl.Free;
        end;
      end;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  //URL(テキスト）のD&D
  else if Succeeded(dataObj.GetData(TextFormatEtc,medium)) then
  begin
    try
      if Assigned(FOnDragDropText) then
        FOnDragDropText(FTarget,pt.x,pt.y,GetText(medium.hGlobal));
    finally
      ReleaseStgMedium(medium);
    end;
  end;
end;

procedure TDragDropTarget.GetFiles(HG: HGlobal; AFiles: TStrings);
var
  pfiles: PDropFiles;
  filename: PChar;
  s: String;
  ws: WideString;
begin
  AFiles.Clear;
  pfiles := PDropFiles(GlobalLock(HG));
  try
    filename := PChar(pfiles) + pfiles^.pFiles;

    while (filename^ <> #0) do
    begin
      if (pfiles^.fWide) then
      begin
        ws := PWideChar(filename);
        Inc(filename,(Length(ws) * 2) + 2);
        s := ws;
      end
      else begin
        s := filename;
        Inc(filename, Length(s) + 1);
      end;

      AFiles.Add(s);
    end;

  finally
    GlobalUnlock(HG);
  end;
end;

function TDragDropTarget.GetText(HG: HGlobal): String;
begin
  Result := PChar(GlobalLock(HG));
  GlobalUnlock(HG);
end;

procedure TDragDropTarget.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  //controlを終了
  if (FTarget = AComponent) and (Operation = opRemove) then
    SetTarget(nil);
end;

procedure TDragDropTarget.SetTarget(const Value: TWinControl);
//targetを登録する
begin
  if FTarget = Value then
    Exit;

  //以前を削除
  if Assigned(FTarget) then
  begin
    //終了通知を削除
    FTarget.RemoveFreeNotification(Self);
    RemoveFreeNotification(FTarget);
    //d&dを削除
    if FTargetHandle <> 0 then
      RevokeDragDrop(FTargetHandle); //失敗checkはしない
  end;

  //入れ替え
  FTarget := Value;
  FTargetHandle := 0;
  //登録する
  if Assigned(FTarget) then
  begin      
    try
      if not (csDesigning in ComponentState) then
      begin
        OleCheck(RegisterDragDrop(FTarget.Handle,Self));
        FTargetHandle := FTarget.Handle;   
      end;
    except
      FTarget := nil;
      raise;
    end;
    
    //終了通知
    FTarget.FreeNotification(Self);
  end;
end;

{ TDragDropSource }

function GetFileListDataObject(ADir: String; AFiles: TStrings): IDataObject;
type
  PArrayOfPItemIDList = ^TArrayOfPItemIDList;
  TArrayOfPItemIDList = array[0..0] of PItemIDList;
var
  Malloc: IMalloc;
  Root: IShellFolder;
  FolderPidl: PItemIDList;
  Folder: IShellFolder;
  p: PArrayOfPItemIDList;
  chEaten: ULONG;
  dwAttributes: ULONG;
  FileCount: Integer;
  i: Integer;
  sl: TStringList;
begin
  Result := nil;
  if AFiles.Count = 0 then
    Exit;

  sl := TStringList.Create;
  try
    //コピー
    sl.Assign(AFiles);
    //dirを得る
    if ADir = '' then
      ADir := ExtractFilePath(sl[0])
    else
      ADir := IncludeTrailingBackslash(ADir);
      
    for i := sl.Count - 1 downto 0 do
      if not FileExists(ADir + ExtractFileName(sl[i])) then
        sl.Delete(i);
    //無い場合は終わり
    if sl.Count = 0 then
      Exit;

    OleCheck(SHGetMalloc(Malloc));
    OleCheck(SHGetDesktopFolder(Root));
    OleCheck(Root.ParseDisplayName(
      0, nil,PWideChar(WideString(ADir)),chEaten,FolderPidl,dwAttributes));
    try
      OleCheck(Root.BindToObject(
        FolderPidl, nil, IShellFolder,Pointer(Folder)));

      FileCount := sl.Count;
      p := AllocMem(SizeOf(PItemIDList) * FileCount);
      try
        for i := 0 to FileCount - 1 do
        begin
          OleCheck(Folder.ParseDisplayName(0, nil,
            PWideChar(WideString(ExtractFileName(sl[i]))),chEaten,p^[i],dwAttributes));
        end;

        OleCheck(Folder.GetUIObjectOf(
          0, FileCount, p^[0], IDataObject, nil,Pointer(Result)));
      finally
        for i := 0 to FileCount - 1 do
          if p^[i] <> nil then
            Malloc.Free(p^[i]);

        FreeMem(p);
      end;
    finally
      Malloc.Free(FolderPidl);
    end;
  finally
    sl.Free;
  end;
end;  

procedure TDragDropSource.DoDragFiles(AFolder: String; AFiles: TStrings);
//ファイルのD&D開始
var
  Effect: Integer;
  data: IDataObject;
begin
  data := GetFileListDataObject(AFolder,AFiles);
  if Assigned(data) then
    DoDragDrop(data,Self,DROPEFFECT_COPY,Effect);
end;

procedure TDragDropSource.DoDragText(AText: String);
//テキストのD&D開始
var
  Effect: Integer;
begin
  DoDragDrop(TDragDropTextDataObject.Create(AText),
             Self,DROPEFFECT_LINK,Effect);
end;

function TDragDropSource.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TDragDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  //escかLB RB同時で中止
  if fEscapePressed or
    ((MK_LBUTTON or MK_RBUTTON) = (grfKeyState and (MK_RBUTTON or MK_RBUTTON))) then
    Result := DRAGDROP_S_CANCEL
  else if ((grfKeyState and (MK_LBUTTON or MK_RBUTTON)) = 0) then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;


{ TDragDropTextDataObject }

constructor TDragDropTextDataObject.Create(AText: TDragDropStr);
begin
  inherited Create;
  FText := AText;
end;

function TDragDropTextDataObject.DAdvise(const formatetc: TFormatEtc;
  advf: Integer; const advSink: IAdviseSink;
  out dwConnection: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.DUnadvise(dwConnection: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.EnumDAdvise(
  out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.EnumFormatEtc(dwDirection: Integer;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.GetCanonicalFormatEtc(
  const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
var
  p: PChar;
begin
  medium.tymed := 0;
  medium.UnkForRelease := nil;
  medium.hGlobal := 0;
  //チェック
  Result := QueryGetData(formatetcIn);
  if Failed(Result) then
    Exit;

  medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND,Length(FText) + 1);
  //メモリ確保失敗
  if medium.hGlobal = 0 then
    Result := E_OUTOFMEMORY
  else begin
    //文字列を返す
    medium.tymed := TYMED_HGLOBAL;
    p := PChar(GlobalLock(medium.hGlobal));
    try
      StrPCopy(p,FText);
    finally
      GlobalUnlock(medium.hGlobal);
    end;
  end; 
end;

function TDragDropTextDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDragDropTextDataObject.QueryGetData(
  const formatetc: TFormatEtc): HResult;
//問い合わせをチェック
begin
  if (formatetc.cfFormat = CF_TEXT) and
     (formatetc.dwAspect = DVASPECT_CONTENT) and
     (formatetc.tymed and TYMED_HGLOBAL <> 0) then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;

function TDragDropTextDataObject.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize;


end.