{
 File: States.inc

 ISO 3166, 3166-1, 3166-2 & 3166-3 implementation part

 Author : Frédéric Libaud (http://www.libaudfrederic.fr)

 Licence : LGPL V3.0+

 =============================================================================
 history
 -----------------------------------------------------------------------------
}
type
  TISO31663 = array [0..2] of char;
  TISO31662 = array [0..1] of char;

  TStateLanguageTableRecord = record
    ISO31663: TISO31663;
    Name: string;
  end;



