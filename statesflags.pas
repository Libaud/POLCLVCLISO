unit StatesFlags;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

{$R StatesFlags.res}

uses
  Classes, SysUtils, Graphics, ImgList;

{$i States.inc}

procedure StatesFlags(aState: TISO31663; var aPNG: TPortableNetworkGraphic);

procedure LoadStatesFlags(aImageList: TCustomImageList);

implementation

uses
  ExtendStrings, States;

procedure StatesFlags(aState: TISO31663; var aPNG: TPortableNetworkGraphic);
begin
  if assigned(aPNG) and not IsEmpty(aState) then
  begin
    {$ifdef TYPHON}
    aPNG.LoadFromTyphonResource(aState);
    {$endif}
  end;
end;

procedure LoadStatesFlags(aImageList: TCustomImageList);
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    {$ifdef TYPHON}
    aImageList.AddTyphonResource(StatesCodesTable[iItem].ISO31663);
    {$endif}
end;

end.

