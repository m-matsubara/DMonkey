unit gSocketHtml;
{html tag解析}

interface

uses
  Windows,SysUtils,Classes,gsocketmisc,hashtable
{$IFDEF WS2}
  ,winsock2;
{$ELSE}
  ,Winsock;
{$ENDIF}

type
  EHtmlTagError = class(Exception);

  THtmlTag = class(TObject)
  private
    FName: String;
    FDict: THashTable;

    procedure ParseTag(Tag: String);
    function GetKeys: String;
    function GetValue(const Key: String): String;
    //procedure SetValue(const Key, Value: String);
  public
    constructor Create(const Tag: String);
    destructor Destroy; override;

    property Name: String read FName;
    property Keys: String read GetKeys;
    property Values[const Key: String]: String read GetValue; default;
  end;

  THtmlParser = class(TObject)
  private
    FTags: TList;
    FText: String;

    function GetCount: Integer;
    function GetTag(Index: Integer): THtmlTag;
  public
    constructor Create(const Html: String);
    destructor Destroy; override;
    procedure Parse(S: String);
    procedure Clear;

    property Count: Integer read GetCount;
    property Tags[Index: Integer]: THtmlTag read GetTag; default;
    property Text: String read FText;
  end;



implementation

{ THtmlTag }

constructor THtmlTag.Create(const Tag: String);
//生成
begin
  inherited Create;
  FDict := THashTable.Create(HASH_50,True);
  //FDict.IgnoreCase := True;
  ParseTag(Tag);
end;

destructor THtmlTag.Destroy;
//破棄
begin
  FDict.Free;
  inherited Destroy;
end;

function THtmlTag.GetKeys: String;
//keyを全て返す
begin
  Result := FDict.Keys;
end;

function THtmlTag.GetValue(const Key: String): String;
//valueを返す
begin
  Result := '';
  try
    Result := FDict[Key];
  except
  end;
end;
{
procedure THtmlTag.SetValue(const Key, Value: String);
//valueをセット
begin
  FDict[Key] := Value;
end;
 }
procedure THtmlTag.ParseTag(Tag: String);
//tag解析
var
  i: Integer;
  IsName,IsKey,IsValue,IsQuote: Boolean;
  AKey,AValue,Quote: String;
begin
  Tag := RemoveCRLF(Tag);
  //&amp;は&に変換される
  Tag := StringReplace(Tag,'&amp;','&',[rfReplaceAll, rfIgnoreCase]);
  
  IsName := False;
  IsKey := False;
  IsValue := False;
  IsQuote := False;
  AKey := '';
  AValue := '';
  Quote := ' ';
  for i := 1 to Length(Tag) do
  begin
    //tag start
    if Tag[i] = '<' then
      IsName := True
    //tag name
    else if IsName then
    begin
      //最初の空白はcancel
      if (FName = '') and (Tag[i] = ' ') then
        Continue
      //空白で終り
      else if (Tag[i] = ' ') or (Tag[i] = '>') then
      begin
        //小文字に
        FName := LowerCase(Trim(FName));
        //&quot;は"に変換される
        FName := StringReplace(FName,'&quot;','"',[rfReplaceAll, rfIgnoreCase]);
        IsName := False;
        //key on
        IsKey := True;
        Continue;
      end
      else
        FName := FName + Tag[i];
    end
    //tag key
    else if IsKey then
    begin
      //最初の空白はcancel
      if (AKey = '') and (Tag[i] = ' ') then
        Continue
      // =で終り
      else if (Tag[i] = '=') or (Tag[i] = '>') then
      begin
        AKey := LowerCase(Trim(AKey));
        //&quot;は"に変換される
        AKey := StringReplace(AKey,'&quot;','"',[rfReplaceAll, rfIgnoreCase]);
        IsKey := False;
        IsValue := True;
        IsQuote := False;
        Continue;
      end
      else
        AKey := AKey + Tag[i];
    end
    //tag value
    else if IsValue then
    begin
      //最初の空白はcancel
      if (AValue = '') and (Tag[i] = ' ') then
        Continue
      //終り
      else if (Quote = Tag[i]) or (Tag[i] = '>') then
      begin
        AValue := Trim(AValue);
        //&quot;は"に変換される
        AValue := StringReplace(AValue,'&quot;','"',[rfReplaceAll, rfIgnoreCase]);
        FDict[AKey] := AValue;
        //AddKey(AKey,AValue);
        IsKey := True;
        IsValue := False;
        IsQuote := False;
        AKey := '';
        AValue := '';
        Quote := ' ';
        Continue;
      end
      else begin
        if IsQuote then
          AValue := AValue + Tag[i]
        //区切りを調べる
        else begin
          if Tag[i] = '"' then
            Quote := '"'
          else if Tag[i] = SINGLE_QUOTE then
            Quote := SINGLE_QUOTE
          else if Tag[i] = ' 'then
            Quote := ' '
          else begin
            Quote := ' ';
            AValue := Tag[i];
          end;
          IsQuote := True;
        end;
      end;
    end;

  end;

end;

{ THtmlPaser }

procedure THtmlParser.Clear;
var
  i: Integer;
begin
  for i := FTags.Count - 1 downto 0 do
    TObject(FTags[i]).Free;

  FTags.Clear;
end;

constructor THtmlParser.Create(const Html: String);
begin
  inherited Create;
  FTags := TList.Create;
  Parse(Html);
end;

destructor THtmlParser.Destroy;
begin
  Clear;  
  FreeAndNil(FTags);
  inherited Destroy;
end;

function THtmlParser.GetCount: Integer;
begin
  Result := FTags.Count;
end;

function THtmlParser.GetTag(Index: Integer): THtmlTag;
begin
  Result := nil;
  if (Index > -1) and (Index < FTags.Count) then
    Result := THtmlTag(FTags[Index]);
end;

procedure THtmlParser.Parse(S: String);
var
  i: Integer;
  IsTag: Boolean;
  Tag: String;
begin
  Clear;

  IsTag := False;
  Tag := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = '<' then
    begin
      IsTag := True;
      Tag := S[i];
    end
    else if S[i] = '>' then
    begin
      IsTag := False;
      Tag := Tag + S[i];
      //改行チェック
      if LowerCase(Tag) = '<br>' then
        FText := FText + CRLF
      else
        FTags.Add(THtmlTag.Create(Tag));
    end
    else if IsTag then
      Tag := Tag + S[i]
    else
      FText := FText + S[i];
  end;  
end;



end.
