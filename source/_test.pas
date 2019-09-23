unit _test;

interface

uses
  Windows, Messages, SysUtils, Classes,
  ecma_type,ecma_object;

type
  //TJObjectを継承します
  TTestObject = class(TJObject) 
  private
    FCells: array[0..10] of array[0..10] of String;
    FDate: TJDateObject;      
    function GetApplicationFilename: String;
    //メソッドは TJMethod型である必要があります
    function DoAdd(Param: TJValueList): TJValue;
    function DoCells(Param: TJValueList): TJValue;
  protected
    procedure Notification(AObject: TJNotify); override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    //propertyはpublishedに設定します
    property applicationFilename: String read GetApplicationFilename;
    property date: TJDateObject read FDate;
  end;   

implementation

{ TTestObject }

constructor TTestObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  //objectの名前を設定します
  RegistName('Test');
  //メソッドを登録します
  RegistMethod('add',DoAdd);
  RegistMethod('Cells',DoCells);

  //Date Objectを作成します
  //※RegisteringFactoryはFalseにしてFactoryに登録しない
  FDate := TJDateObject.Create(AEngine,nil,False);
  {または、登録する場合には終了通知を受ける
  FDate := TJDateObject.Create(AEngine);
  FDate.FreeNotification(Self);
  }
  //参照カウントを増やす
  FDate.IncRef;    
end;

procedure TTestObject.Notification(AObject: TJNotify);
//終了通知を受ける
begin
  inherited;
  {終了通知を受けた場合はFDateをnilにする
  if AObject = FDate then
    FDate := nil;
  }
end;

destructor TTestObject.Destroy;
begin
  //参照カウントを減らす(解放はしない)
  FDate.DecRef;
  {終了通知を受けた場合はFDateがnilになっている可能性がある
  if Assigned(FDate) then
    FDate.DecRef;
  }
  inherited;
end;

function TTestObject.DoAdd(Param: TJValueList): TJValue;
var
  v: TJValue;
  i,ret: Integer;
begin
  //全ての引数を加算する
  ret := 0;
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      //TJValueを整数に変換して加算
      Inc(ret,AsInteger(@v));
    end;
  //整数をTJValueに変換する
  Result := BuildInteger(ret);
end;

function TTestObject.DoCells(Param: TJValueList): TJValue;
var
  row,col: Integer;
  v: TJValue;
begin
  EmptyValue(Result);
  //引数をチェック
  if IsParam2(Param) then
  begin
    v := Param[0];
    row := AsInteger(@v);
    v := Param[1];
    col := AsInteger(@v);
    //もう一つあればセットする
    if IsParam3(Param) then
    begin
      //setter
      Result := Param[2];
      FCells[row][col] := AsString(@Result);
    end
    else begin
      //getter
      //s := Format('%s(%d,%d) = %s',['Cells',row,col,FCells[row][col]]);
      Result := BuildString(FCells[row][col]);
    end;
  end;
end;

function TTestObject.GetApplicationFilename: String;
begin
  //普通に文字列を返すとTJValueに変換されます
  Result := ParamStr(0);
end;




end.
