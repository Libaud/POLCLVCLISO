{

 ITU (International Telecommunication Union) E.164 recommandation implementation


}
unit StatesPhones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, States;

type
  TStatePhoneRecord = record
    StateCode: TISO31663;
    PhoneCode: word;
  end;

const
  StatesPhonesTable: array [0..248] of TStatePhoneRecord =
  (
   {000}(StateCode:'AFG'; PhoneCode: 93),   { Afghanistan }
   {001}(StateCode:'ZAF'; PhoneCode: 27),   { South Africa }
   {002}(StateCode:'ALA'; PhoneCode: 358),  { Åland Islands }
   {003}(StateCode:'ALB'; PhoneCode: 355),  { Albania }
   {004}(StateCode:'DZA'; PhoneCode: 213),  { Algeria }
   {005}(StateCode:'DEU'; PhoneCode: 49),   { Germany }
   {006}(StateCode:'AND'; PhoneCode: 376),  { Andorra }
   {007}(StateCode:'AGO'; PhoneCode: 244),  { Angola }
   {008}(StateCode:'AIA'; PhoneCode: 1264), { Anguilla }
   {009}(StateCode:'ATG'; PhoneCode: 1268), { Antigua and Barbuda }
   {010}(StateCode:'ATA'; PhoneCode: 672),  { Antarctica }
   {011}(StateCode:'SAU'; PhoneCode: 966),  { Saudi Arabia }
   {012}(StateCode:'ARG'; PhoneCode: 54),   { Argentina }
   {013}(StateCode:'ARM'; PhoneCode: 374),  { Armenia }
   {014}(StateCode:'ABW'; PhoneCode: 297),  { Aruba }
   {015}(StateCode:'SHN'; PhoneCode: 247),  { Saint Helena, Ascension and Tristan da Cunha }
   {016}(StateCode:'AUS'; PhoneCode: 61),   { Australia }
   {017}(StateCode:'AUT'; PhoneCode: 43),   { Austria }
   {018}(StateCode:'AZE'; PhoneCode: 994),  { Azerbaijan }
   {019}(StateCode:'BHS'; PhoneCode: 1242), { Bahamas }
   {020}(StateCode:'BHR'; PhoneCode: 973),  { Bahrain }
   {021}(StateCode:'BGD'; PhoneCode: 880),  { Bangladesh }
   {022}(StateCode:'BRB'; PhoneCode: 1246), { Barbados }
   {023}(StateCode:'BEL'; PhoneCode: 32),   { Belgium }
   {024}(StateCode:'BLZ'; PhoneCode: 501),  { Belize }
   {025}(StateCode:'BEN'; PhoneCode: 229),  { Benin }
   {026}(StateCode:'BMU'; PhoneCode: 1441), { Bermuda }
   {027}(StateCode:'BTN'; PhoneCode: 975),  { Bhutan }
   {028}(StateCode:'BLR'; PhoneCode: 375),  { Belarus }
   {029}(StateCode:'MMR'; PhoneCode: 95),   { Myanmar }
   {030}(StateCode:'BOL'; PhoneCode: 591),  { Bolivia }
   {031}(StateCode:'BIH'; PhoneCode: 387),  { Bosnia and Herzegovina }
   {032}(StateCode:'BWA'; PhoneCode: 267),  { Botswana }
   {033}(StateCode:'BVT'; PhoneCode: 47),   { Bouvet Island }
   {034}(StateCode:'BRA'; PhoneCode: 55),   { Brazil }
   {035}(StateCode:'BRN'; PhoneCode: 673),  { Brunei Darussalam }
   {036}(StateCode:'BGR'; PhoneCode: 359),  { Bulgaria }
   {037}(StateCode:'BFA'; PhoneCode: 226),  { Burkina Faso }
   {038}(StateCode:'BDI'; PhoneCode: 257),  { Burundi }
   {039}(StateCode:'KHM'; PhoneCode: 855),  { Cambodia }
   {040}(StateCode:'CMR'; PhoneCode: 237),  { Cameroon}
   {041}(StateCode:'CAN'; PhoneCode: 1),    { Canada }
   {042}(StateCode:'CPD'; PhoneCode: 238),  { Cabo Verde }
   {043}(StateCode:'CYM'; PhoneCode: 1345), { Cayman Islands }
   {044}(StateCode:'CAF'; PhoneCode: 236),  { Central African Republic }
   {045}(StateCode:'CHL'; PhoneCode: 56),   { Chile }
   {046}(StateCode:'CHN'; PhoneCode: 86),   { China }
   {047}(StateCode:'CYP'; PhoneCode: 357),  { Cyprus }
   {048}(StateCode:'COL'; PhoneCode: 57),   { Colombia }
   {049}(StateCode:'COM'; PhoneCode: 269),  { Comoros }
   {050}(StateCode:'COD'; PhoneCode: 243),  { Congo (Democratic Republic of the) }
   {051}(StateCode:'COG'; PhoneCode: 242),  { Congo }
   {052}(StateCode:'COK'; PhoneCode: 682),  { Cook Islands }
   {053}(StateCode:'PRK'; PhoneCode: 850),  { Korea (Democratic People''s Republic of) }
   {054}(StateCode:'KOR'; PhoneCode: 82),   { Korea (Republic of) }
   {055}(StateCode:'CRI'; PhoneCode: 506),  { Costa Rica }
   {056}(StateCode:'CIV'; PhoneCode: 225),  { Côte d''Ivoire }
   {057}(StateCode:'HRV'; PhoneCode: 385),  { Croatia }
   {058}(StateCode:'CUB'; PhoneCode: 53),   { Cuba }
   {059}(StateCode:'CUW'; PhoneCode: 599),  { Curaçao }
   {060}(StateCode:'DNK'; PhoneCode: 45),   { Denmark }
   {061}(StateCode:'DJI'; PhoneCode: 253),  { Djibouti }
   {062}(StateCode:'DOM'; PhoneCode: 18),   { Dominican Republic}
   {063}(StateCode:'DMA'; PhoneCode: 1767), { Dominica }
   {064}(StateCode:'EGY'; PhoneCode: 20),   { Egypt }
   {065}(StateCode:'ARE'; PhoneCode: 971),  { United Arab Emirates }
   {066}(StateCode:'ECU'; PhoneCode: 593),  { Ecuador }
   {067}(StateCode:'ERI'; PhoneCode: 291),  { Eritrea }
   {068}(StateCode:'ESP'; PhoneCode: 34),   { Spain }
   {069}(StateCode:'EST'; PhoneCode: 372),  { Estonia }
   {070}(StateCode:'USA'; PhoneCode: 1),    { United States of America }
   {071}(StateCode:'ETH'; PhoneCode: 251),  { Ethiopia }
   {072}(StateCode:'FRO'; PhoneCode: 298),  { Faroe Islands }
   {073}(StateCode:'FJI'; PhoneCode: 679),  { Fiji }
   {074}(StateCode:'FIN'; PhoneCode: 358),  { Finland }
   {075}(StateCode:'FRA'; PhoneCode: 33),   { France}
   {076}(StateCode:'GAB'; PhoneCode: 241),  { Gabon }
   {077}(StateCode:'GMB'; PhoneCode: 220),  { Gambia }
   {078}(StateCode:'GEO'; PhoneCode: 995),  { Georgia }
   {079}(StateCode:'SGS'; PhoneCode: 500),  { South Georgia and the South Sandwich Islands }
   {080}(StateCode:'GHA'; PhoneCode: 233),  { Ghana }
   {081}(StateCode:'GIB'; PhoneCode: 350),  { Gibraltar }
   {082}(StateCode:'GRC'; PhoneCode: 30),   { Greece }
   {083}(StateCode:'GRD'; PhoneCode: 1473), { Grenada}
   {084}(StateCode:'GRL'; PhoneCode: 299),  { Greenland }
   {085}(StateCode:'GLP'; PhoneCode: 590),  { Guadeloupe }
   {086}(StateCode:'GUM'; PhoneCode: 1671), { Guam }
   {087}(StateCode:'GTM'; PhoneCode: 502),  { Guatemala }
   {088}(StateCode:'GGY'; PhoneCode: 44),   { Guernsey }
   {089}(StateCode:'GIN'; PhoneCode: 224),  { Guinea }
   {090}(StateCode:'GNQ'; PhoneCode: 240),  { Equatorial Guinea }
   {091}(StateCode:'GNB'; PhoneCode: 245),  { Guinea-Bissau }
   {092}(StateCode:'GUY'; PhoneCode: 592),  { Guyana }
   {093}(StateCode:'GUF'; PhoneCode: 594),  { French Guiana }
   {094}(StateCode:'HTI'; PhoneCode: 509),  { Haiti }
   {095}(StateCode:'HND'; PhoneCode: 504),  { Honduras }
   {096}(StateCode:'HKG'; PhoneCode: 852),  { Hong Kong }
   {097}(StateCode:'HUN'; PhoneCode: 36),   { Hungary }
   {098}(StateCode:'IMN'; PhoneCode: 44),   { Isle of Man }
   {099}(StateCode:'IND'; PhoneCode: 91),   { India }
   {100}(StateCode:'IDN'; PhoneCode: 62),   { Indonesia }
   {101}(StateCode:'IRQ'; PhoneCode: 964),  { Iraq }
   {102}(StateCode:'IRN'; PhoneCode: 98),   { Iran }
   {103}(StateCode:'IRL'; PhoneCode: 353),  { Ireland }
   {104}(StateCode:'ISL'; PhoneCode: 354),  { Iceland }
   {105}(StateCode:'ISR'; PhoneCode: 972),  { Israel }
   {106}(StateCode:'ITA'; PhoneCode: 39),   { Italy }
   {107}(StateCode:'JAM'; PhoneCode: 1876), { Jamaica }
   {108}(StateCode:'JPN'; PhoneCode: 81),   { Japan }
   {109}(StateCode:'JEY'; PhoneCode: 44),   { Jersey }
   {110}(StateCode:'JOR'; PhoneCode: 962),  { Jordan }
   {111}(StateCode:'KAZ'; PhoneCode: 7),    { Kazakhstan }
   {112}(StateCode:'KEN'; PhoneCode: 254),  { Kenya }
   {113}(StateCode:'KGZ'; PhoneCode: 996),  { Kyrgyzstan }
   {114}(StateCode:'KIR'; PhoneCode: 686),  { Kiribati }
   {115}(StateCode:''; PhoneCode: 383),     { Kosovo }
   {116}(StateCode:'KWT'; PhoneCode: 965),  { Kuwait }
   {117}(StateCode:'LAO'; PhoneCode: 856),  { Lao People''s Democratic Republic }
   {118}(StateCode:'LSO'; PhoneCode: 266),  { Lesotho }
   {119}(StateCode:'LVA'; PhoneCode: 371),  { Latvia }
   {120}(StateCode:'LBN'; PhoneCode: 961),  { Lebanon }
   {121}(StateCode:'LBR'; PhoneCode: 231),  { Liberia}
   {122}(StateCode:'LBY'; PhoneCode: 218),  { Libya }
   {123}(StateCode:'LIE'; PhoneCode: 423),  { Liechtenstein }
   {124}(StateCode:'LTU'; PhoneCode: 370),  { Lithuania }
   {125}(StateCode:'LUX'; PhoneCode: 352),  { Luxembourg }
   {126}(StateCode:'MAC'; PhoneCode: 853),  { Macao }
   {127}(StateCode:'MKD'; PhoneCode: 389),  { Macedonia (the former Yugoslav Republic of) }
   {128}(StateCode:'MDG'; PhoneCode: 261),  { Madagascar }
   {129}(StateCode:'MYS'; PhoneCode: 60),   { Malaysia }
   {130}(StateCode:'MWI'; PhoneCode: 265),  { Malawi }
   {131}(StateCode:'MDV'; PhoneCode: 960),  { Maldives }
   {132}(StateCode:'MLI'; PhoneCode: 223),  { Mali }
   {133}(StateCode:'FLK'; PhoneCode: 500),  { Falkland Islands (Malvinas) }
   {134}(StateCode:'MLT'; PhoneCode: 356),  { Malta }
   {135}(StateCode:'MNP'; PhoneCode: 1670), { Northern Mariana Islands }
   {136}(StateCode:'MAR'; PhoneCode: 212),  { Morocco }
   {137}(StateCode:'MHL'; PhoneCode: 692),  { Marshall Islands }
   {138}(StateCode:'MTQ'; PhoneCode: 596),  { Martinique }
   {139}(StateCode:'MUS'; PhoneCode: 230),  { Mauritius }
   {140}(StateCode:'MRT'; PhoneCode: 222),  { Mauritania }
   {141}(StateCode:'MYT'; PhoneCode: 262),  { Mayotte }
   {142}(StateCode:'MEX'; PhoneCode: 52),   { Mexico }
   {143}(StateCode:'FSM'; PhoneCode: 691),  { Micronesia (Federated States of) }
   {144}(StateCode:'MDA'; PhoneCode: 373),  { Moldova (Republic of) }
   {145}(StateCode:'MCO'; PhoneCode: 377),  { Monaco }
   {146}(StateCode:'MNG'; PhoneCode: 976),  { Mongolia }
   {147}(StateCode:'MNE'; PhoneCode: 382),  { Montenegro }
   {148}(StateCode:'MSR'; PhoneCode: 1664), { Montserrat }
   {149}(StateCode:'MOZ'; PhoneCode: 258),  { Mozambique }
   {150}(StateCode:'NAM'; PhoneCode: 264),  { Namibia }
   {151}(StateCode:'NRU'; PhoneCode: 674),  { Nauru }
   {152}(StateCode:'NPL'; PhoneCode: 977),  { Nepal }
   {153}(StateCode:'NIC'; PhoneCode: 505),  { Nicaragua }
   {154}(StateCode:'NER'; PhoneCode: 227),  { Niger }
   {155}(StateCode:'NGA'; PhoneCode: 234),  { Nigeria }
   {156}(StateCode:'NIU'; PhoneCode: 683),  { Niue }
   {157}(StateCode:'NFK'; PhoneCode: 6723), { Norfolk Island }
   {158}(StateCode:'NOR'; PhoneCode: 47),   { Norway }
   {159}(StateCode:'NCL'; PhoneCode: 687),  { New Caledonia }
   {160}(StateCode:'NZL'; PhoneCode: 64),   { New Zealand }
   {161}(StateCode:'OMN'; PhoneCode: 968),  { Oman }
   {162}(StateCode:'UGA'; PhoneCode: 256),  { Uganda }
   {163}(StateCode:'UZB'; PhoneCode: 998),  { Uzbekistan }
   {164}(StateCode:'PAK'; PhoneCode: 92),   { Pakistan }
   {165}(StateCode:'PLW'; PhoneCode: 680),  { Palau }
   {166}(StateCode:'PSE'; PhoneCode: 970),  { Palestine, State of }
   {167}(StateCode:'PAN'; PhoneCode: 507),  { Panama }
   {168}(StateCode:'PNG'; PhoneCode: 675),  { Papua New Guinea }
   {169}(StateCode:'PRY'; PhoneCode: 595),  { Paraguay }
   {170}(StateCode:'NLD'; PhoneCode: 31),   { Netherlands }
   {171}(StateCode:'BES'; PhoneCode: 599),  { Bonaire, Sint Eustatius and Saba }
   {172}(StateCode:'PER'; PhoneCode: 51),   { Peru }
   {173}(StateCode:'PHL'; PhoneCode: 63),   { Philippines }
   {174}(StateCode:'PCN'; PhoneCode: 64),   { Pitcairn }
   {175}(StateCode:'POL'; PhoneCode: 48),   { Poland }
   {176}(StateCode:'PYF'; PhoneCode: 689),  { French Polynesia }
   {177}(StateCode:'PRI'; PhoneCode: 1787), { Puerto Rico }
   {178}(StateCode:'PRT'; PhoneCode: 351),  { Portugal }
   {179}(StateCode:'QAT'; PhoneCode: 974),  { Qatar }
   {180}(StateCode:'REU'; PhoneCode: 262),  { Réunion }
   {181}(StateCode:'ROU'; PhoneCode: 40),   { Romania }
   {182}(StateCode:'GBR'; PhoneCode: 44),   { United Kingdom of Great Britain and Northern Ireland }
   {183}(StateCode:'RUS'; PhoneCode: 7),    { Russian Federation }
   {184}(StateCode:'RWA'; PhoneCode: 250),  { Rwanda }
   {185}(StateCode:'ESH'; PhoneCode: 212),  { Western Sahara }
   {186}(StateCode:'BLM'; PhoneCode: 590),  { Saint Barthélemy }
   {187}(StateCode:'KNA'; PhoneCode: 1869), { Saint Kitts and Nevis }
   {188}(StateCode:'SHN'; PhoneCode: 290),  { Saint Helena, Ascension and Tristan da Cunha }
   {189}(StateCode:'LCA'; PhoneCode: 1758), { Saint Lucia }
   {190}(StateCode:'SMR'; PhoneCode: 378),  { San Marino }
   {191}(StateCode:'MAF'; PhoneCode: 590),  { Saint Martin (French part) }
   {192}(StateCode:'SPM'; PhoneCode: 508),  { Saint Pierre and Miquelon }
   {193}(StateCode:'VCT'; PhoneCode: 1784), { Saint Vincent and the Grenadines }
   {194}(StateCode:'SLB'; PhoneCode: 677),  { Solomon Islands }
   {195}(StateCode:'SLV'; PhoneCode: 503),  { El Salvador }
   {196}(StateCode:'WSM'; PhoneCode: 685),  { Samoa }
   {197}(StateCode:'ASM'; PhoneCode: 1684), { American Samoa }
   {198}(StateCode:'STP'; PhoneCode: 239),  { Sao Tome and Principe}
   {199}(StateCode:'SEN'; PhoneCode: 221),  { Senegal }
   {200}(StateCode:'SRB'; PhoneCode: 381),  { Serbia }
   {201}(StateCode:'SYC'; PhoneCode: 248),  { Seychelles }
   {202}(StateCode:'SLE'; PhoneCode: 232),  { Sierra Leone }
   {203}(StateCode:'SGP'; PhoneCode: 65),   { Singapore }
   {204}(StateCode:'SXM'; PhoneCode: 1721), { Sint Maarten (Dutch part) }
   {205}(StateCode:'SVK'; PhoneCode: 421),  { Slovakia }
   {206}(StateCode:'SVN'; PhoneCode: 386),  { Slovenia }
   {207}(StateCode:'SOM'; PhoneCode: 252),  { Somalia }
   {208}(StateCode:'SDN'; PhoneCode: 249),  { Sudan }
   {209}(StateCode:'SSD'; PhoneCode: 211),  { South Sudan }
   {210}(StateCode:'LKA'; PhoneCode: 94),   { Sri Lanka }
   {211}(StateCode:'SWE'; PhoneCode: 46),   { Sweden }
   {212}(StateCode:'CHE'; PhoneCode: 41),   { Switzerland }
   {213}(StateCode:'SUR'; PhoneCode: 597),  { Suriname }
   {214}(StateCode:'SJM'; PhoneCode: 47),   { Svalbard and Jan Mayen }
   {215}(StateCode:'SWZ'; PhoneCode: 268),  { Swaziland }
   {216}(StateCode:'SYR'; PhoneCode: 963),  { Syrian Arab Republic }
   {217}(StateCode:'TJK'; PhoneCode: 992),  { Tajikistan}
   {218}(StateCode:'TZA'; PhoneCode: 255),  { Tanzania, United Republic of }
   {219}(StateCode:'TWN'; PhoneCode: 886),  { Taiwan, Province of China }
   {220}(StateCode:'TCD'; PhoneCode: 235),  { Chad }
   {221}(StateCode:'CZE'; PhoneCode: 420),  { Czechia}
   {221}(StateCode:'ATF'; PhoneCode: 262),  { French Southern Territories }
   {222}(StateCode:'IOT'; PhoneCode: 246),  { British Indian Ocean Territory }
   {223}(StateCode:''; PhoneCode: 672), {}
   {224}(StateCode:'THA'; PhoneCode: 66),   { Thailand }
   {225}(StateCode:'TLS'; PhoneCode: 670),  { Timor-Leste }
   {226}(StateCode:'TGO'; PhoneCode: 228),  { Togo }
   {227}(StateCode:'TKL'; PhoneCode: 690),  { Tokelau }
   {229}(StateCode:'TON'; PhoneCode: 676),  { Tonga }
   {230}(StateCode:'TTO'; PhoneCode: 1868), { Trinidad and Tobago }
   {231}(StateCode:'TUN'; PhoneCode: 216),  { Tunisia }
   {232}(StateCode:'TKM'; PhoneCode: 993),  { Turkmenistan }
   {233}(StateCode:'TCA'; PhoneCode: 1649), { Turks and Caicos Islands }
   {234}(StateCode:'TUR'; PhoneCode: 90),   { Turkey }
   {235}(StateCode:'TUV'; PhoneCode: 688),  { Tuvalu }
   {236}(StateCode:'UKR'; PhoneCode: 380),  { Ukraine }
   {237}(StateCode:'URY'; PhoneCode: 598),  { Uruguay }
   {238}(StateCode:'VUT'; PhoneCode: 678),  { Vanuatu }
   {239}(StateCode:'VAT'; PhoneCode: 379),  { Holy See }
   {240}(StateCode:'VEN'; PhoneCode: 58),   { Venezuela (Bolivarian Republic of) }
   {241}(StateCode:'VIR'; PhoneCode: 1340), { Virgin Islands (U.S.) }
   {242}(StateCode:'VGB'; PhoneCode: 1284), { Virgin Islands (British) }
   {243}(StateCode:'VNM'; PhoneCode: 84),   { Viet Nam }
   {244}(StateCode:'WLF'; PhoneCode: 681),  { Wallis and Futuna }
   {245}(StateCode:'YEM'; PhoneCode: 697),  { Yemen }
   {246}(StateCode:'ZMB'; PhoneCode: 260),  { Zambia }
   {247}(StateCode:'ZWE'; PhoneCode: 263),  { Zimbabwe }
   {248}(StateCode:'PRI'; PhoneCode: 1939) { Puerto Rico (Second) }
  );

function StateToPhoneCode(aStateCode: TISO31663): word;
function PhoneToStateCode(aPhoneCode: word): TISO31663;

implementation

function StateToPhoneCode(aStateCode: TISO31663): word;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesPhonesTable) -1 do
    if StatesPhonesTable[iItem].StateCode = aStateCode then
    begin
      Result:= StatesPhonesTable[iItem].PhoneCode;
      break;
    end;
end;

function PhoneToStateCode(aPhoneCode: word): TISO31663;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesPhonesTable) -1 do
    if StatesPhonesTable[iItem].PhoneCode = aPhoneCode then
    begin
      Result:= StatesPhonesTable[iItem].StateCode;
      break;
    end;
end;

end.

