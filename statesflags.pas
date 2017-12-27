{
 Unit : StatesFlags.pas

 ISO 3166, 3166-1, 3166-2 & 3166-3 implementation parts. States flags.

 Author : Frédéric Libaud (http://www.libaudfrederic.fr)

 Licence : LGPL V3.0+

 =============================================================================
 history
 -----------------------------------------------------------------------------
}
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

