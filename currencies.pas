{
 Unit : Currencies.pas

 Description : ISO 4217 implementation

 Author : Frédéric Libaud (http://www.libaudfrederic.fr)

 Licence : LGPL V3.0+

 =============================================================================
 history
 -----------------------------------------------------------------------------
}
unit Currencies;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, States;

{$i States.inc}

const
  CURRENCIE_DOLLAR  = '$';
  CURRENCIE_EURO    = '€';
  CURRENCIE_POUND   = '£';
  CURRENCIE_YUANYEN = '¥';
  CURRENCIE_FRANC   = 'F';

type
  TCurrencieCode = array [0..2] of char;

  TCurrencieSymbol = (csNone, csDollar, csEURO, csPOUND, csYUANYEN, csFRANC);

  TCurrencie = record
    Currencie: TCurrencieCode;
    State: TISO31663;
    Symbol: TCurrencieSymbol;
    Replaced: TCurrencieCode;
  end;

const
  CurrenciesStatesTable: array [0..343] of TCurrencie =
  (
   {000}(Currencie: 'AFA'; State: 'AFG'; Symbol: csNone; Replaced: 'AFN'), { afghani }
   {001}(Currencie: 'AFN'; State: 'AFG'; Symbol: csNone; Replaced: ''),    { afghani }
   {002}(Currencie: 'XAG'; State: ''; Symbol: csNone; Replaced: ''),       { argent }
   {003}(Currencie: 'MGA'; State: 'MDG'; Symbol: csNone; Replaced: ''),    { ariary }
   {004}(Currencie: 'THB'; State: 'THA'; Symbol: csNone; Replaced: ''),    { baht }
   {005}(Currencie: 'PAB'; State: 'PAN'; Symbol: csNone; Replaced: ''),    { balboa }
   {006}(Currencie: 'ETB'; State: 'ETH'; Symbol: csNone; Replaced: ''),    { birr }
   {007}(Currencie: 'VEB'; State: 'VEN'; Symbol: csNone; Replaced: 'VEF'), { bolivar }
   {008}(Currencie: 'VEF'; State: 'VEN'; Symbol: csNone; Replaced: ''),    { bolivar fuerté }
   {009}(Currencie: 'BOB'; State: 'BOL'; Symbol: csNone; Replaced: ''),    { boliviano }
   {010}(Currencie: 'GHS'; State: 'GHA'; Symbol: csNone; Replaced: ''),    { cedi }
   {011}(Currencie: 'CRC'; State: 'CRI'; Symbol: csNone; Replaced: ''),    { colon }
   {012}(Currencie: 'SVC'; State: 'SLV'; Symbol: csNone; Replaced: 'USD'), { colon }
   {013}(Currencie: 'NIC'; State: 'NIC'; Symbol: csNone; Replaced: 'NIO'), { cordoba }
   {014}(Currencie: 'NIO'; State: 'NIC'; Symbol: csNone; Replaced: ''),    { cordoba d'or }
   {015}(Currencie: 'DKK'; State: 'DNK'; Symbol: csNone; Replaced: ''),    { couronne }
   {016}(Currencie: 'DKK'; State: 'FRO'; Symbol: csNone; Replaced: ''),    { couronne }
   {017}(Currencie: 'DKK'; State: 'GRL'; Symbol: csNone; Replaced: ''),    { couronne }
   {018}(Currencie: 'EEK'; State: 'EST'; Symbol: csNone; Replaced: 'EUR'), { couronne }
   {019}(Currencie: 'ISK'; State: 'ISL'; Symbol: csNone; Replaced: ''),    { couronne }
   {020}(Currencie: 'NOK'; State: 'NOR'; Symbol: csNone; Replaced: ''),    { couronne }
   {021}(Currencie: 'NOK'; State: 'BVT'; Symbol: csNone; Replaced: ''),    { couronne }
   {022}(Currencie: 'NOK'; State: 'SJM'; Symbol: csNone; Replaced: ''),    { courrone }
   {023}(Currencie: 'SKK'; State: 'SVK'; Symbol: csNone; Replaced: 'EUR'), { couronne }
   {020}(Currencie: 'SEK'; State: 'SWE'; Symbol: csNone; Replaced: ''),    { couronne }
   {021}(Currencie: 'CZK'; State: ''; Symbol: csNone; Replaced: ''),       { couronne }
   {022}(Currencie: 'CSK'; State: ''; Symbol: csNone; Replaced: ''),       { couronne }
   {023}(Currencie: 'GMD'; State: 'GMB'; Symbol: csNone; Replaced: ''),    { dalasi }
   {024}(Currencie: 'MKD'; State: 'MKD'; Symbol: csNone; Replaced: ''),    { denar }
   {025}(Currencie: 'DZD'; State: 'DZA'; Symbol: csNone; Replaced: ''),    { dinar }
   {026}(Currencie: 'BHD'; State: 'BHR'; Symbol: csNone; Replaced: ''),    { dinar }
   {027}(Currencie: 'IQD'; State: 'IRQ'; Symbol: csNone; Replaced: ''),    { dinar }
   {028}(Currencie: 'JOD'; State: 'JOR'; Symbol: csNone; Replaced: ''),    { dinar }
   {029}(Currencie: 'KWD'; State: 'KWT'; Symbol: csNone; Replaced: ''),    { dinar }
   {030}(Currencie: 'LYD'; State: 'LBY'; Symbol: csNone; Replaced: ''),    { dinar }
   {031}(Currencie: 'RSD'; State: 'SRB'; Symbol: csNone; Replaced: ''),    { dinar }
   {032}(Currencie: 'CSD'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {033}(Currencie: 'SDD'; State: 'SDN'; Symbol: csNone; Replaced: ''),    { dinar }
   {034}(Currencie: 'TND'; State: 'TUN'; Symbol: csNone; Replaced: ''),    { dinar }
   {035}(Currencie: 'YUF'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {036}(Currencie: 'YUD'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {037}(Currencie: 'YUN'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {038}(Currencie: 'YUR'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {039}(Currencie: 'YUO'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {040}(Currencie: 'YUG'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {041}(Currencie: 'YUM'; State: ''; Symbol: csNone; Replaced: ''),       { dinar }
   {042}(Currencie: 'AED'; State: 'ARE'; Symbol: csNone; Replaced: ''),    { dirham }
   {043}(Currencie: 'MAD'; State: 'MAR'; Symbol: csNone; Replaced: ''),    { dirham }
   {044}(Currencie: 'STD'; State: 'STP'; Symbol: csNone; Replaced: ''),    { dobra }
   {045}(Currencie: 'AUD'; State: 'AUS'; Symbol: csDollar; Replaced: ''),  { dollar }
   {046}(Currencie: 'AUD'; State: 'CXR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {047}(Currencie: 'AUD'; State: 'CCK'; Symbol: csDollar; Replaced: ''),  { dollar }
   {048}(Currencie: 'AUD'; State: 'HMD'; Symbol: csDollar; Replaced: ''),  { dollar }
   {049}(Currencie: 'AUD'; State: 'KIR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {050}(Currencie: 'AUD'; State: 'NRU'; Symbol: csDollar; Replaced: ''),  { dollar }
   {051}(Currencie: 'AUD'; State: 'NFK'; Symbol: csDollar; Replaced: ''),  { dollar }
   {052}(Currencie: 'AUD'; State: 'TUV'; Symbol: csDollar; Replaced: ''),  { dollar }
   {053}(Currencie: 'BSD'; State: 'BHS'; Symbol: csNone; Replaced: ''),    { dollar }
   {054}(Currencie: 'BZD'; State: 'BLZ'; Symbol: csNone; Replaced: ''),    { dollar }
   {055}(Currencie: 'BMD'; State: 'BMU'; Symbol: csNone; Replaced: ''),    { dollar }
   {056}(Currencie: 'BND'; State: 'BRN'; Symbol: csNone; Replaced: ''),    { dollar }
   {057}(Currencie: 'KYD'; State: 'CYM'; Symbol: csNone; Replaced: ''),    { dollar }
   {058}(Currencie: 'CAD'; State: 'CAN'; Symbol: csNone; Replaced: ''),    { dollar }
   {059}(Currencie: 'XCD'; State: 'AIA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {060}(Currencie: 'XCD'; State: 'ATG'; Symbol: csDollar; Replaced: ''),  { dollar }
   {061}(Currencie: 'XCD'; State: 'DMA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {062}(Currencie: 'XCD'; State: 'GRD'; Symbol: csDollar; Replaced: ''),  { dollar }
   {063}(Currencie: 'XCD'; State: 'MSR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {064}(Currencie: 'XCD'; State: 'KNA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {065}(Currencie: 'XCD'; State: 'LCA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {066}(Currencie: 'XCD'; State: 'VCT'; Symbol: csDollar; Replaced: ''),  { dollar }
   {067}(Currencie: 'FJD'; State: 'FJI'; Symbol: csDollar; Replaced: ''),  { dollar }
   {068}(Currencie: 'GYD'; State: 'GUY'; Symbol: csDollar; Replaced: ''),  { dollar }
   {069}(Currencie: 'HKD'; State: 'HKG'; Symbol: csDollar; Replaced: ''),  { dollar }
   {070}(Currencie: 'SBD'; State: 'SLB'; Symbol: csDollar; Replaced: ''),  { dollar }
   {071}(Currencie: 'JMD'; State: 'JAM'; Symbol: csDollar; Replaced: ''),  { dollar }
   {072}(Currencie: 'BBD'; State: 'BRB'; Symbol: csDollar; Replaced: ''),  { dollar }
   {073}(Currencie: 'LRD'; State: 'LBR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {074}(Currencie: 'NAD'; State: 'NAM'; Symbol: csDollar; Replaced: ''),  { dollar }
   {075}(Currencie: 'NZD'; State: 'NZL'; Symbol: csDollar; Replaced: ''),  { dollar }
   {076}(Currencie: 'NZD'; State: 'TKL'; Symbol: csDollar; Replaced: ''),  { dollar }
   {077}(Currencie: 'NZD'; State: 'COK'; Symbol: csDollar; Replaced: ''),  { dollar }
   {078}(Currencie: 'NZD'; State: 'NIU'; Symbol: csDollar; Replaced: ''),  { dollar }
   {079}(Currencie: 'NZD'; State: 'PCN'; Symbol: csDollar; Replaced: ''),  { dollar }
   {080}(Currencie: 'SGD'; State: 'SGP'; Symbol: csDollar; Replaced: ''),  { dollar }
   {081}(Currencie: 'SRD'; State: 'SUR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {082}(Currencie: 'TWD'; State: 'TWN'; Symbol: csDollar; Replaced: ''),  { dollar }
   {083}(Currencie: 'TTD'; State: 'TTO'; Symbol: csDollar; Replaced: ''),  { dollar }
   {084}(Currencie: 'USD'; State: 'USA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {085}(Currencie: 'USD'; State: 'MNP'; Symbol: csDollar; Replaced: ''),  { dollar }
   {086}(Currencie: 'USD'; State: 'UMI'; Symbol: csDollar; Replaced: ''),  { dollar }
   {087}(Currencie: 'USD'; State: 'VIR'; Symbol: csDollar; Replaced: ''),  { dollar }
   {088}(Currencie: 'USD'; State: 'PRI'; Symbol: csDollar; Replaced: ''),  { dollar }
   {089}(Currencie: 'USD'; State: 'ASM'; Symbol: csDollar; Replaced: ''),  { dollar }
   {090}(Currencie: 'USD'; State: 'BES'; Symbol: csDollar; Replaced: ''),  { dollar }
   {091}(Currencie: 'USD'; State: 'ECU'; Symbol: csDollar; Replaced: ''),  { dollar }
   {092}(Currencie: 'USD'; State: 'GTM'; Symbol: csDollar; Replaced: ''),  { dollar }
   {093}(Currencie: 'USD'; State: 'HTI'; Symbol: csDollar; Replaced: ''),  { dollar }
   {094}(Currencie: 'USD'; State: 'TCA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {095}(Currencie: 'USD'; State: 'VGB'; Symbol: csDollar; Replaced: ''),  { dollar }
   {097}(Currencie: 'USD'; State: 'MHL'; Symbol: csDollar; Replaced: ''),  { dollar }
   {098}(Currencie: 'USD'; State: 'FSM'; Symbol: csDollar; Replaced: ''),  { dollar }
   {099}(Currencie: 'USD'; State: 'PLW'; Symbol: csDollar; Replaced: ''),  { dollar }
   {100}(Currencie: 'USD'; State: 'PAN'; Symbol: csDollar; Replaced: ''),  { dollar }
   {101}(Currencie: 'USD'; State: 'SLV'; Symbol: csDollar; Replaced: ''),  { dollar }
   {102}(Currencie: 'USD'; State: 'IOT'; Symbol: csDollar; Replaced: ''),  { dollar }
   {103}(Currencie: 'USD'; State: 'TLS'; Symbol: csDollar; Replaced: ''),  { dollar }
   {104}(Currencie: 'USS'; State: 'USA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {105}(Currencie: 'USN'; State: 'USA'; Symbol: csDollar; Replaced: ''),  { dollar }
   {106}(Currencie: 'ZWD'; State: 'ZWE'; Symbol: csDollar; Replaced: ''),  { dollar }
   {107}(Currencie: 'ZWR'; State: 'ZWE'; Symbol: csDollar; Replaced: ''),  { dollar }
   {108}(Currencie: 'ZWL'; State: 'ZWE'; Symbol: csDollar; Replaced: ''),  { dollar }
   {109}(Currencie: 'VND'; State: 'VNM'; Symbol: csNone; Replaced: ''),    { dong }
   {110}(Currencie: 'GRD'; State: 'GRC'; Symbol: csNone; Replaced: 'EUR'), { drachme}
   {111}(Currencie: 'AMD'; State: 'ARM'; Symbol: csNone; Replaced: ''),    { dram }
   {112}(Currencie: 'XDR'; State: ''; Symbol: csNone; Replaced: ''),       { }
   {113}(Currencie: 'CVE'; State: 'CPV'; Symbol: csNone; Replaced: ''),    { escudo }
   {114}(Currencie: 'MZE'; State: 'MOZ'; Symbol: csNone; Replaced: ''),    { escudo }
   {115}(Currencie: 'PTE'; State: 'PRT'; Symbol: csNone; Replaced: ''),    { escudo }
   {116}(Currencie: 'PTE'; State: 'TLS'; Symbol: csNone; Replaced: 'TPE'), { escudo }
   {117}(Currencie: 'TPE'; State: 'TLS'; Symbol: csNone; Replaced: 'IDR'), { escudo }
   {118}(Currencie: 'EUR'; State: 'DEU'; Symbol: csEuro; Replaced: ''),    { euro }
   {119}(Currencie: 'EUR'; State: 'AUT'; Symbol: csEuro; Replaced: ''),    { euro }
   {120}(Currencie: 'EUR'; State: 'BEL'; Symbol: csEuro; Replaced: ''),    { euro }
   {121}(Currencie: 'EUR'; State: 'CYP'; Symbol: csEuro; Replaced: ''),    { euro }
   {122}(Currencie: 'EUR'; State: 'ESP'; Symbol: csEuro; Replaced: ''),    { euro }
   {123}(Currencie: 'EUR'; State: 'EST'; Symbol: csEuro; Replaced: ''),    { euro }
   {124}(Currencie: 'EUR'; State: 'FIN'; Symbol: csEuro; Replaced: ''),    { euro }
   {125}(Currencie: 'EUR'; State: 'ALA'; Symbol: csEuro; Replaced: ''),    { euro }
   {126}(Currencie: 'EUR'; State: 'FRA'; Symbol: csEuro; Replaced: ''),    { euro }
   {127}(Currencie: 'EUR'; State: 'GRC'; Symbol: csEuro; Replaced: ''),    { euro }
   {128}(Currencie: 'EUR'; State: 'IRL'; Symbol: csEuro; Replaced: ''),    { euro }
   {129}(Currencie: 'EUR'; State: 'ITA'; Symbol: csEuro; Replaced: ''),    { euro }
   {130}(Currencie: 'EUR'; State: 'LVA'; Symbol: csEuro; Replaced: ''),    { euro }
   {131}(Currencie: 'EUR'; State: 'LTU'; Symbol: csEuro; Replaced: ''),    { euro }
   {132}(Currencie: 'EUR'; State: 'LUX'; Symbol: csEuro; Replaced: ''),    { euro }
   {133}(Currencie: 'EUR'; State: 'MLT'; Symbol: csEuro; Replaced: ''),    { euro }
   {134}(Currencie: 'EUR'; State: 'NLD'; Symbol: csEuro; Replaced: ''),    { euro }
   {135}(Currencie: 'EUR'; State: 'PRT'; Symbol: csEuro; Replaced: ''),    { euro }
   {136}(Currencie: 'EUR'; State: 'SVK'; Symbol: csEuro; Replaced: ''),    { euro }
   {137}(Currencie: 'EUR'; State: 'SVN'; Symbol: csEuro; Replaced: ''),    { euro }
   {138}(Currencie: 'EUR'; State: 'AND'; Symbol: csEuro; Replaced: ''),    { euro }
   {139}(Currencie: 'EUR'; State: 'MCO'; Symbol: csEuro; Replaced: ''),    { euro }
   {140}(Currencie: 'EUR'; State: 'MNE'; Symbol: csEuro; Replaced: ''),    { euro }
   {141}(Currencie: 'EUR'; State: 'BLM'; Symbol: csEuro; Replaced: ''),    { euro }
   {142}(Currencie: 'EUR'; State: 'SMR'; Symbol: csEuro; Replaced: ''),    { euro }
   {143}(Currencie: 'EUR'; State: 'ATF'; Symbol: csEuro; Replaced: ''),    { euro }
   {144}(Currencie: 'EUR'; State: 'VAT'; Symbol: csEuro; Replaced: ''),    { euro }
   {145}(Currencie: 'ANG'; State: 'ABW'; Symbol: csNone; Replaced: 'USD'), { florin }
   {146}(Currencie: 'ANG'; State: 'BES'; Symbol: csNone; Replaced: 'USD'), { florin }
   {147}(Currencie: 'ANG'; State: 'CUW'; Symbol: csNone; Replaced: ''), { florin }
   {148}(Currencie: 'ANG'; State: 'SXW'; Symbol: csNone; Replaced: ''), { florin }
   {149}(Currencie: 'AWG'; State: 'ABW'; Symbol: csNone; Replaced: ''),    { florin }
   {150}(Currencie: 'NLG'; State: 'NLD'; Symbol: csNone; Replaced: 'EUR'), { florin }
   {151}(Currencie: 'HUF'; State: 'HUN'; Symbol: csNone; Replaced: ''),    { florint }
   {152}(Currencie: 'ADF'; State: 'AND'; Symbol: csNone; Replaced: 'EUR'), { franc }
   {153}(Currencie: 'BEF'; State: 'BEL'; Symbol: csNone; Replaced: 'EUR'), { franc }
   {154}(Currencie: 'BIF'; State: 'BDI'; Symbol: csNone; Replaced: ''),    { franc }
   {155}(Currencie: 'NHF'; State: 'VUT'; Symbol: csNone; Replaced: 'VUV'), { franc }
   {156}(Currencie: 'KMF'; State: 'COM'; Symbol: csNone; Replaced: ''),    { franc }
   {157}(Currencie: 'CDF'; State: 'COD'; Symbol: csNone; Replaced: ''),    { franc }
   {158}(Currencie: 'DJF'; State: 'DJI'; Symbol: csNone; Replaced: ''),    { franc }
   {159}(Currencie: 'FRF'; State: 'FRA'; Symbol: csNone; Replaced: 'EUR'), { franc }
   {160}(Currencie: 'GNF'; State: 'GIN'; Symbol: csNone; Replaced: ''),    { franc }
   {161}(Currencie: 'LUF'; State: 'LUX'; Symbol: csNone; Replaced: 'EUR'), { franc }
   {162}(Currencie: 'MGF'; State: 'MDG'; Symbol: csNone; Replaced: 'MGA'), { franc }
   {163}(Currencie: 'RWF'; State: 'RWA'; Symbol: csNone; Replaced: ''),    { franc }
   {164}(Currencie: 'CHF'; State: 'CHE'; Symbol: csNone; Replaced: ''),    { franc }
   {165}(Currencie: 'CHF'; State: 'LIE'; Symbol: csNone; Replaced: ''),    { franc }
   {166}(Currencie: 'XOF'; State: 'BEN'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {167}(Currencie: 'XOF'; State: 'BFA'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {168}(Currencie: 'XOF'; State: 'CIV'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {169}(Currencie: 'XOF'; State: 'GNB'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {170}(Currencie: 'XOF'; State: 'MLI'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {171}(Currencie: 'XOF'; State: 'NER'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {172}(Currencie: 'XOF'; State: 'SEN'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {173}(Currencie: 'XOF'; State: 'TGO'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {174}(Currencie: 'XAF'; State: 'CMR'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {175}(Currencie: 'XAF'; State: 'CAF'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {176}(Currencie: 'XAF'; State: 'COG'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {177}(Currencie: 'XAF'; State: 'GAB'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {178}(Currencie: 'XAF'; State: 'GNQ'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {179}(Currencie: 'XAF'; State: 'TCD'; Symbol: csNone; Replaced: ''),    { franc CFA }
   {180}(Currencie: 'XPF'; State: 'NCL'; Symbol: csNone; Replaced: ''),    { franc CFP }
   {181}(Currencie: 'XPF'; State: 'PYF'; Symbol: csNone; Replaced: ''),    { franc CFP }
   {182}(Currencie: 'XPF'; State: 'WLF'; Symbol: csNone; Replaced: ''),    { franc CFP }
   {183}(Currencie: 'XFO'; State: ''; Symbol: csNone; Replaced: ''),       { franc OR }
   {184}(Currencie: 'XFU'; State: ''; Symbol: csNone; Replaced: 'EUR'),    { franc UIC }
   {185}(Currencie: 'CHW'; State: 'CHE'; Symbol: csNone; Replaced: ''),    { franc WIR}
   {186}(Currencie: 'HTG'; State: 'HTI'; Symbol: csNone; Replaced: ''),    { gourde }
   {187}(Currencie: 'PYG'; State: 'PRY'; Symbol: csNone; Replaced: ''),    { guarani }
   {188}(Currencie: 'UAH'; State: 'UKR'; Symbol: csNone; Replaced: ''),    { hryvnia }
   {189}(Currencie: 'PGK'; State: 'PNG'; Symbol: csNone; Replaced: ''),       { kina }
   {190}(Currencie: 'LAK'; State: 'LAO'; Symbol: csNone; Replaced: ''),       { kip }
   {191}(Currencie: 'HRK'; State: 'HRV'; Symbol: csNone; Replaced: ''),       { kuna }
   {192}(Currencie: 'MWK'; State: 'MWI'; Symbol: csNone; Replaced: ''),       { kwacha }
   {193}(Currencie: 'ZMK'; State: 'ZMB'; Symbol: csNone; Replaced: 'ZMW'),       { kwacha }
   {194}(Currencie: 'ZMW'; State: 'ZMB'; Symbol: csNone; Replaced: ''),       { kwacha }
   {195}(Currencie: 'AOA'; State: 'AGO'; Symbol: csNone; Replaced: ''),       { kwanza }
   {195}(Currencie: 'AOK'; State: 'AGO'; Symbol: csNone; Replaced: 'AON'),       { kwanza }
   {196}(Currencie: 'AON'; State: 'AGO'; Symbol: csNone; Replaced: 'AOA'),       { kwanza }
   {197}(Currencie: 'AOR'; State: 'AGO'; Symbol: csNone; Replaced: 'AOA'),       { kwanza }
   {198}(Currencie: 'MMK'; State: 'MMR'; Symbol: csNone; Replaced: ''),       { kyat }
   {199}(Currencie: 'GEL'; State: 'GEO'; Symbol: csNone; Replaced: ''),       { lari }
   {200}(Currencie: 'LVL'; State: 'LVA'; Symbol: csNone; Replaced: 'EUR'),    { lats }
   {201}(Currencie: 'ALL'; State: 'ALB'; Symbol: csNone; Replaced: ''),       { lek }
   {202}(Currencie: 'HNL'; State: 'HND'; Symbol: csNone; Replaced: ''),       { lempira }
   {203}(Currencie: 'SLL'; State: 'SLE'; Symbol: csNone; Replaced: ''),       { leone }
   {204}(Currencie: 'MDL'; State: 'MDA'; Symbol: csNone; Replaced: ''),       { leu }
   {205}(Currencie: 'ROL'; State: 'ROU'; Symbol: csNone; Replaced: 'RON'),    { leu }
   {206}(Currencie: 'RON'; State: 'ROU'; Symbol: csNone; Replaced: ''),       { leu }
   {207}(Currencie: 'BGJ'; State: 'BGR'; Symbol: csNone; Replaced: 'BGK'),    { lev }
   {208}(Currencie: 'BGK'; State: 'BGR'; Symbol: csNone; Replaced: 'BGL'),    { lev }
   {209}(Currencie: 'BGL'; State: 'BGR'; Symbol: csNone; Replaced: 'BGN'),    { lev }
   {210}(Currencie: 'BGN'; State: 'BGR'; Symbol: csNone; Replaced: ''),       { lev }
   {211}(Currencie: 'SZL'; State: 'SWZ'; Symbol: csNone; Replaced: ''),       { lilangeni }
   {212}(Currencie: 'MTL'; State: 'MLT'; Symbol: csNone; Replaced: 'EUR'),    { lire }
   {213}(Currencie: 'ITL'; State: 'ITA'; Symbol: csNone; Replaced: 'EUR'),    { lire }
   {214}(Currencie: 'SML'; State: 'SMR'; Symbol: csNone; Replaced: 'EUR'),    { lire }
   {215}(Currencie: 'VAL'; State: 'VAT'; Symbol: csNone; Replaced: 'EUR'),    { lire }
   {216}(Currencie: 'LTL'; State: 'LTU'; Symbol: csNone; Replaced: 'EUR'),    { litas }
   {217}(Currencie: 'CYP'; State: 'CYP'; Symbol: csNone; Replaced: 'EUR'),    { livre }
   {218}(Currencie: 'EGP'; State: 'EGY'; Symbol: csNone; Replaced: ''),       { livre }
   {219}(Currencie: 'FKP'; State: 'FLK'; Symbol: csNone; Replaced: ''),       { livre }
   {220}(Currencie: 'FKP'; State: 'SGS'; Symbol: csNone; Replaced: ''),       { livre }
   {221}(Currencie: 'GIP'; State: 'GIB'; Symbol: csNone; Replaced: ''),       { livre }
   {222}(Currencie: 'IEP'; State: 'IRL'; Symbol: csNone; Replaced: 'EUR'),    { livre }
   {223}(Currencie: 'LBP'; State: 'LBN'; Symbol: csNone; Replaced: ''),       { livre }
   {224}(Currencie: 'SHP'; State: 'SHN'; Symbol: csNone; Replaced: ''),       { livre }
   {225}(Currencie: 'SDP'; State: 'SDN'; Symbol: csNone; Replaced: 'SDD'),    { livre }
   {226}(Currencie: 'SDG'; State: 'SDN'; Symbol: csNone; Replaced: ''),       { livre }
   {227}(Currencie: 'SSP'; State: 'SSD'; Symbol: csNone; Replaced: ''),       { livre }
   {228}(Currencie: 'SYP'; State: 'SYR'; Symbol: csNone; Replaced: ''),       { livre }
   {229}(Currencie: 'TRL'; State: 'TUR'; Symbol: csNone; Replaced: 'TRY'),    { livre }
   {230}(Currencie: 'TRY'; State: 'TUR'; Symbol: csNone; Replaced: ''),       { livre }
   {231}(Currencie: 'GBP'; State: 'GBR'; Symbol: csNone; Replaced: ''),       { livre sterling }
   {232}(Currencie: 'LSL'; State: 'LSO'; Symbol: csNone; Replaced: ''),       { loti }
   {233}(Currencie: 'AZM'; State: 'AZE'; Symbol: csNone; Replaced: 'AZN'),    { manat }
   {234}(Currencie: 'AZN'; State: 'AZE'; Symbol: csNone; Replaced: ''),       { manat }
   {235}(Currencie: 'TMM'; State: 'TKM'; Symbol: csNone; Replaced: 'TMT'),    { manat }
   {236}(Currencie: 'TMT'; State: 'TKM'; Symbol: csNone; Replaced: ''),       { manat }
   {237}(Currencie: 'DEM'; State: 'DEU'; Symbol: csNone; Replaced: 'EUR'),    { mark }
   {238}(Currencie: 'DEM'; State: 'BIH'; Symbol: csNone; Replaced: 'BAM'),    { mark }
   {239}(Currencie: 'BAM'; State: 'BIH'; Symbol: csNone; Replaced: ''),       { marka }
   {240}(Currencie: 'FIM'; State: 'FIN'; Symbol: csNone; Replaced: 'EUR'),    { markka }
   {241}(Currencie: 'MZM'; State: 'MOZ'; Symbol: csNone; Replaced: 'MZN'),    { metical }
   {242}(Currencie: 'MZN'; State: 'MOZ'; Symbol: csNone; Replaced: ''),       { metical }
   {243}(Currencie: 'BOV'; State: 'BOL'; Symbol: csNone; Replaced: ''),       { mvdol }
   {244}(Currencie: 'ERN'; State: 'ERI'; Symbol: csNone; Replaced: ''),       { nakfa }
   {245}(Currencie: 'NGN'; State: 'NGA'; Symbol: csNone; Replaced: ''),       { naira }
   {246}(Currencie: 'BTN'; State: 'BTN'; Symbol: csNone; Replaced: ''),       { ngultrum }
   {247}(Currencie: 'XAU'; State: ''; Symbol: csNone; Replaced: ''),       { or }
   {248}(Currencie: 'MRO'; State: 'MRT'; Symbol: csNone; Replaced: ''),       { ouguiya }
   {249}(Currencie: 'TOP'; State: 'TON'; Symbol: csNone; Replaced: ''),       { pa’anga }
   {250}(Currencie: 'XPD'; State: ''; Symbol: csNone; Replaced: ''),       { palladium }
   {251}(Currencie: 'MOP'; State: 'MAC'; Symbol: csNone; Replaced: ''),       { pataca }
   {252}(Currencie: 'ADP'; State: 'AND'; Symbol: csNone; Replaced: 'EUR'),    { peseta }
   {253}(Currencie: 'ESP'; State: 'ESP'; Symbol: csNone; Replaced: 'EUR'),    { peseta }
   {254}(Currencie: 'ARP'; State: 'ARG'; Symbol: csNone; Replaced: 'ARS'),       { peso }
   {255}(Currencie: 'ARS'; State: 'ARG'; Symbol: csNone; Replaced: ''),       { peso }
   {256}(Currencie: 'BOP'; State: 'BOL'; Symbol: csNone; Replaced: 'BOB'),       { peso }
   {257}(Currencie: 'CLP'; State: 'CHL'; Symbol: csNone; Replaced: ''),       { peso }
   {258}(Currencie: 'COP'; State: 'COL'; Symbol: csNone; Replaced: ''),       { peso }
   {259}(Currencie: 'CUP'; State: 'CUB'; Symbol: csNone; Replaced: ''),       { peso }
   {260}(Currencie: 'CUC'; State: 'CUB'; Symbol: csNone; Replaced: ''),       { peso }
   {261}(Currencie: 'GWP'; State: 'GNB'; Symbol: csNone; Replaced: 'XOF'),       { peso }
   {262}(Currencie: 'MXN'; State: 'MEX'; Symbol: csNone; Replaced: ''),       { peso }
   {263}(Currencie: 'PHP'; State: 'PHL'; Symbol: csNone; Replaced: ''),       { peso }
   {264}(Currencie: 'DOP'; State: 'DOM'; Symbol: csNone; Replaced: ''),       { peso }
   {265}(Currencie: 'UYI'; State: 'URY'; Symbol: csNone; Replaced: 'UYU'),       { peso }
   {266}(Currencie: 'UYU'; State: 'URY'; Symbol: csNone; Replaced: ''),       { peso }
   {267}(Currencie: 'XPT'; State: ''; Symbol: csNone; Replaced: ''),          { platine }
   {268}(Currencie: 'BWP'; State: 'BWA'; Symbol: csNone; Replaced: ''),       { pula }
   {269}(Currencie: 'GTQ'; State: 'GTM'; Symbol: csNone; Replaced: ''),       { quetzal }
   {270}(Currencie: 'ZAR'; State: 'ZAF'; Symbol: csNone; Replaced: ''),       { rand }
   {271}(Currencie: 'ZAR'; State: 'NAM'; Symbol: csNone; Replaced: ''),       { rand }
   {272}(Currencie: 'BRR'; State: 'BRA'; Symbol: csNone; Replaced: 'BRL'),    { réal }
   {273}(Currencie: 'BRL'; State: 'BRA'; Symbol: csNone; Replaced: ''),       { réal }
   {274}(Currencie: 'IRR'; State: 'IRN'; Symbol: csNone; Replaced: ''),       { rial }
   {275}(Currencie: 'OMR'; State: 'OMN'; Symbol: csNone; Replaced: ''),       { rial }
   {276}(Currencie: 'QAR'; State: 'QAT'; Symbol: csNone; Replaced: ''),       { rial }
   {277}(Currencie: 'KHR'; State: 'KHM'; Symbol: csNone; Replaced: ''),       { riel }
   {278}(Currencie: 'MYR'; State: 'MYS'; Symbol: csNone; Replaced: ''),       { ringgit }
   {279}(Currencie: 'SAR'; State: 'SAU'; Symbol: csNone; Replaced: ''),       { riyal }
   {280}(Currencie: 'YER'; State: 'YEM'; Symbol: csNone; Replaced: ''),       { rial }
   {281}(Currencie: 'LVR'; State: 'LVA'; Symbol: csNone; Replaced: 'LVL'),    { rouble }
   {282}(Currencie: 'BYB'; State: 'BLR'; Symbol: csNone; Replaced: 'BYR'),    { rouble }
   {283}(Currencie: 'BYR'; State: 'BLR'; Symbol: csNone; Replaced: 'BYN'),    { rouble }
   {284}(Currencie: 'BYN'; State: 'BLR'; Symbol: csNone; Replaced: ''),       { rouble }
   {285}(Currencie: 'SUR'; State: ''; Symbol: csNone; Replaced: ''),       { rouble } { Todo: A comléter }
   {286}(Currencie: 'SUB'; State: 'ARM'; Symbol: csNone; Replaced: 'AMD'),       { rouble }
   {287}(Currencie: 'SUB'; State: 'AZE'; Symbol: csNone; Replaced: 'AZM'),       { rouble }
   {288}(Currencie: 'SUB'; State: 'BLR'; Symbol: csNone; Replaced: 'BYR'),       { rouble }
   {289}(Currencie: 'SUB'; State: 'GEO'; Symbol: csNone; Replaced: 'GEL'),       { rouble }
   {290}(Currencie: 'SUB'; State: 'KAZ'; Symbol: csNone; Replaced: 'KZT'),       { rouble }
   {291}(Currencie: 'SUB'; State: 'UZB'; Symbol: csNone; Replaced: 'UZS'),       { rouble }
   {292}(Currencie: 'SUB'; State: 'RUS'; Symbol: csNone; Replaced: 'RUB'),       { rouble }
   {293}(Currencie: 'SUB'; State: 'TJK'; Symbol: csNone; Replaced: 'TJS'),       { rouble }
   {294}(Currencie: 'SUB'; State: 'TKM'; Symbol: csNone; Replaced: 'TMM'),       { rouble }
   {295}(Currencie: 'SUB'; State: 'UKR'; Symbol: csNone; Replaced: 'UAH'),       { rouble }
   {296}(Currencie: 'RUB'; State: 'RUS'; Symbol: csNone; Replaced: ''),       { rouble }
   {297}(Currencie: 'MUR'; State: 'MUS'; Symbol: csNone; Replaced: ''),       { roupie }
   {298}(Currencie: 'INR'; State: 'IND'; Symbol: csNone; Replaced: ''),       { roupie }
   {299}(Currencie: 'IDR'; State: 'IDN'; Symbol: csNone; Replaced: ''),       { roupie }
   {300}(Currencie: 'IDR'; State: 'TLS'; Symbol: csNone; Replaced: 'USD'),    { roupie }
   {301}(Currencie: 'NPR'; State: 'NPL'; Symbol: csNone; Replaced: ''),       { roupie }
   {302}(Currencie: 'PKR'; State: 'PAK'; Symbol: csNone; Replaced: ''),       { roupie}
   {303}(Currencie: 'SCR'; State: 'SYC'; Symbol: csNone; Replaced: ''),       { roupie }
   {304}(Currencie: 'LKR'; State: 'LKA'; Symbol: csNone; Replaced: ''),       { roupie }
   {305}(Currencie: 'MVR'; State: 'MDV'; Symbol: csNone; Replaced: ''),       { rufiyaa }
   {306}(Currencie: 'ATS'; State: 'AUT'; Symbol: csNone; Replaced: 'EUR'),    { schilling }
   {307}(Currencie: 'KES'; State: 'KEN'; Symbol: csNone; Replaced: ''),       { schilling }
   {308}(Currencie: 'UGX'; State: 'UGA'; Symbol: csNone; Replaced: ''),       { schilling }
   {309}(Currencie: 'SOS'; State: 'SOM'; Symbol: csNone; Replaced: ''),       { schilling }
   {310}(Currencie: 'TZS'; State: 'TZA'; Symbol: csNone; Replaced: ''),       { schilling }
   {311}(Currencie: 'ILS'; State: 'ISR'; Symbol: csNone; Replaced: ''),       { shekel }
   {312}(Currencie: 'ILS'; State: 'PSE'; Symbol: csNone; Replaced: ''),       { shekel }
   {313}(Currencie: 'PES'; State: 'PER'; Symbol: csNone; Replaced: 'PEN'),    { sol }
   {314}(Currencie: 'PEN'; State: 'PER'; Symbol: csNone; Replaced: ''),       { sol }
   {315}(Currencie: 'KGS'; State: 'KGZ'; Symbol: csNone; Replaced: ''),       { som }
   {316}(Currencie: 'TJS'; State: 'TJK'; Symbol: csNone; Replaced: ''),       { somoni }
   {317}(Currencie: 'ECS'; State: 'ECU'; Symbol: csNone; Replaced: 'USD'),    { sucre }
   {318}(Currencie: 'UZS'; State: 'UZB'; Symbol: csNone; Replaced: ''),       { sum }
   {319}(Currencie: 'BDT'; State: 'BGD'; Symbol: csNone; Replaced: ''),       { taka }
   {320}(Currencie: 'WST'; State: 'WSM'; Symbol: csNone; Replaced: ''),    { tala }
   {321}(Currencie: 'KZT'; State: 'KAZ'; Symbol: csNone; Replaced: ''),    { tenge }
   {322}(Currencie: 'SIT'; State: 'SVN'; Symbol: csNone; Replaced: 'EUR'),    { tolar }
   {323}(Currencie: 'MNT'; State: 'MNG'; Symbol: csNone; Replaced: ''),    { tugrik }
   {324}(Currencie: 'CLF'; State: 'CHL'; Symbol: csNone; Replaced: ''),    { unité d'investissement }
   {325}(Currencie: 'XBC'; State: ''; Symbol: csNone; Replaced: 'XBD'),    { unité compte }
   {326}(Currencie: 'XBD'; State: ''; Symbol: csNone; Replaced: 'XEU'),    { unité compte }
   {327}(Currencie: 'XEU'; State: ''; Symbol: csNone; Replaced: 'EUR'),    { unité compte }
   {328}(Currencie: 'MXV'; State: 'MEX'; Symbol: csNone; Replaced: ''),    { unité de conversion }
   {329}(Currencie: 'ECV'; State: 'ECU'; Symbol: csNone; Replaced: ''),    { unité de valeur}
   {330}(Currencie: 'COU'; State: 'COL'; Symbol: csNone; Replaced: ''),    { unité de valeur}
   {331}(Currencie: 'XBA'; State: ''; Symbol: csNone; Replaced: 'XBB'),    { unité européenne }
   {332}(Currencie: 'XBB'; State: ''; Symbol: csNone; Replaced: 'XBC'),    { unité monétaire }
   {333}(Currencie: 'VUV'; State: 'VUT'; Symbol: csNone; Replaced: ''),    { vatu }
   {334}(Currencie: 'KPW'; State: 'PRK'; Symbol: csNone; Replaced: ''),    { won }
   {335}(Currencie: 'KRW'; State: 'KOR'; Symbol: csNone; Replaced: ''),    { won }
   {336}(Currencie: 'JPY'; State: 'JPN'; Symbol: csNone; Replaced: ''),    { yen }
   {337}(Currencie: 'CNY'; State: 'CHN'; Symbol: csNone; Replaced: ''),    { yuan }
   {338}(Currencie: 'PLZ'; State: 'POL'; Symbol: csNone; Replaced: 'PLN'), { zloty }
   {339}(Currencie: 'PLN'; State: 'POL'; Symbol: csNone; Replaced: '')     { zloty }
  );

function CurrencieSymbol(aCurrencieSymbol: TCurrencieSymbol): string;

function CurrencieConvert(aSource: System.Currency; aRate: real): System.Currency;

function CurrencieISO2Name(aISO: TCurrencieCode): string;

procedure CurrencieList(var aList: TStringList);

procedure CurrencieState(aCurrencie: TCurrencieCode; var aStates: TStringList);

implementation

function CurrencieSymbol(aCurrencieSymbol: TCurrencieSymbol): string;
begin
  case aCurrencieSymbol of
    csNone: Result:= #0;
    csDollar: Result:= CURRENCIE_DOLLAR;
    csEURO: Result:= CURRENCIE_EURO;
    csPOUND: Result:= CURRENCIE_POUND;
    csYUANYEN: Result:= CURRENCIE_YUANYEN;
    csFRANC: Result:= CURRENCIE_FRANC;
  end;
end;

function CurrencieConvert(aSource: System.Currency; aRate: real): System.Currency;
begin
  Result:= aSource * aRate;
end;

function CurrencieISO2Name(aISO: TCurrencieCode): string;
var
  iCurrencie: integer;
begin
  for iCurrencie:= 0 to 59 do
    if CurrenciesStatesTable[iCurrencie].Currencie = aISO then
      Result:= CurrenciesStatesTable[iCurrencie].Currencie;
end;

procedure CurrencieList(var aList: TStringList);
var
  iItem: integer;
begin
  for iItem:= 0 to Length(CurrenciesStatesTable) - 1 do
     aList.Add(CurrenciesStatesTable[iItem].Currencie);
end;

procedure CurrencieState(aCurrencie: TCurrencieCode; var aStates: TStringList);
var
  iItem: integer;
begin
  for iItem:= 0 to Length(CurrenciesStatesTable) - 1 do
    if CurrenciesStatesTable[iItem].Currencie = aCurrencie then
      aStates.Add(CurrenciesStatesTable[iItem].State);
end;

end.

