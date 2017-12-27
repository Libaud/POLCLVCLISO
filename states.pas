{
  Unit : States.pas

  ISO 3166, 3166-1, 3166-2 & 3166-3 implementation

  Author : Frédéric Libaud (http://www.libaudfrederic.fr)

  Licence : LGPL V3.0+

  =============================================================================
  history
  -----------------------------------------------------------------------------
}
unit States;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$i States.inc}

type
  TISO3166N = array [0..2] of char;

  TStateRecord = record
    ISO3166N: TISO3166N;
    ISO31663: TISO31663;
    ISO31662: TISO31662;
  end;

const
  {
   Array const that contains states codes (number, 3 chars and 2 chars)
  }
  StatesCodesTable: array [0..248] of TStateRecord =
  (
   {000}(ISO3166N: '004'; ISO31663: 'AFG'; ISO31662: 'AF'), { Afghanistan | Afghanistan }
   {001}(ISO3166N: '710'; ISO31663: 'ZAF'; ISO31662: 'ZA'), { South Africa | Afrique du Sud }
   {002}(ISO3166N: '248'; ISO31663: 'ALA'; ISO31662: 'AX'), { Åland Islands | Îles Åland }
   {003}(ISO3166N: '008'; ISO31663: 'ALB'; ISO31662: 'AL'), { Albania | Albanie }
   {004}(ISO3166N: '012'; ISO31663: 'DZA'; ISO31662: 'DZ'), { Algeria | Algérie }
   {005}(ISO3166N: '276'; ISO31663: 'DEU'; ISO31662: 'DE'), { Germany | Allemagne }
   {006}(ISO3166N: '020'; ISO31663: 'AND'; ISO31662: 'AD'), { Andorra | Andorre }
   {007}(ISO3166N: '024'; ISO31663: 'AGO'; ISO31662: 'AO'), { Angola | Angola }
   {008}(ISO3166N: '660'; ISO31663: 'AIA'; ISO31662: 'AI'), { Anguilla | Anguilla }
   {009}(ISO3166N: '010'; ISO31663: 'ATA'; ISO31662: 'AQ'), { Antartica | Antarctique }
   {010}(ISO3166N: '028'; ISO31663: 'ATG'; ISO31662: 'AG'), { Antigua and Barbuda | Antigua-et-Barbuda }
   {011}(ISO3166N: '682'; ISO31663: 'SAU'; ISO31662: 'SA'), { Saudi Arabia | Arabie saoudite }
   {012}(ISO3166N: '032'; ISO31663: 'ARG'; ISO31662: 'AR'), { Argentina | Argentine }
   {013}(ISO3166N: '051'; ISO31663: 'ARM'; ISO31662: 'AM'), { Armenia | Arménie }
   {014}(ISO3166N: '533'; ISO31663: 'ABW'; ISO31662: 'AW'), { Aruba | Aruba }
   {015}(ISO3166N: '036'; ISO31663: 'AUS'; ISO31662: 'AU'), { Australia | Australie }
   {016}(ISO3166N: '040'; ISO31663: 'AUT'; ISO31662: 'AT'), { Austria | Autriche }
   {017}(ISO3166N: '031'; ISO31663: 'AZE'; ISO31662: 'AZ'), { Azerbaijan | Azerbaïdjan }
   {018}(ISO3166N: '044'; ISO31663: 'BHS'; ISO31662: 'BS'), { Bahamas | Bahamas }
   {019}(ISO3166N: '048'; ISO31663: 'BHR'; ISO31662: 'BH'), { Bahain | Bahrein }
   {020}(ISO3166N: '050'; ISO31663: 'BGD'; ISO31662: 'BD'), { Bangladesh | Bangladesh }
   {021}(ISO3166N: '052'; ISO31663: 'BRB'; ISO31662: 'BB'), { Barbados | Barbade }
   {022}(ISO3166N: '112'; ISO31663: 'BLR'; ISO31662: 'BY'), { Belarus | Biélorussie }
   {023}(ISO3166N: '056'; ISO31663: 'BEL'; ISO31662: 'BE'), { Belgium | Belgique }
   {024}(ISO3166N: '084'; ISO31663: 'BLZ'; ISO31662: 'BZ'), { Belize | Belize }
   {025}(ISO3166N: '204'; ISO31663: 'BEN'; ISO31662: 'BJ'), { Benin | Bénin }
   {026}(ISO3166N: '060'; ISO31663: 'BMU'; ISO31662: 'BM'), { Bermuda | Bermudes }
   {027}(ISO3166N: '064'; ISO31663: 'BTN'; ISO31662: 'BT'), { Bhutan | Bhoutan }
   {028}(ISO3166N: '068'; ISO31663: 'BOL'; ISO31662: 'BO'), { Bolivia | Bolivie }
   {029}(ISO3166N: '535'; ISO31663: 'BES'; ISO31662: 'BQ'), { Bonnaire, Sint Estatius and Saba | Pays-Bas caribéens }
   {030}(ISO3166N: '070'; ISO31663: 'BIH'; ISO31662: 'BA'), { Bosnia and Herzegovina | Bosnie-Herzégovine }
   {031}(ISO3166N: '072'; ISO31663: 'BWA'; ISO31662: 'BW'), { Botswana | Botswana }
   {032}(ISO3166N: '074'; ISO31663: 'BVT'; ISO31662: 'BV'), { Bouvet Island | Ile Bouvet }
   {033}(ISO3166N: '076'; ISO31663: 'BRA'; ISO31662: 'BR'), { Brazil | Brésil }
   {034}(ISO3166N: '096'; ISO31663: 'BRN'; ISO31662: 'BN'), { Brunei Darussalam | Brunei }
   {035}(ISO3166N: '100'; ISO31663: 'BGR'; ISO31662: 'BG'), { Bulgaria | Bulgarie }
   {036}(ISO3166N: '854'; ISO31663: 'BFA'; ISO31662: 'BF'), { Burkina Faso | Burkina Faso }
   {037}(ISO3166N: '108'; ISO31663: 'BDI'; ISO31662: 'BI'), { Burundi | Burundi }
   {038}(ISO3166N: '136'; ISO31663: 'CYM'; ISO31662: 'KY'), { Caymain Islands | Iles Caïmans }
   {039}(ISO3166N: '116'; ISO31663: 'KHM'; ISO31662: 'KH'), { Cambodia | Cambodge }
   {040}(ISO3166N: '120'; ISO31663: 'CMR'; ISO31662: 'CM'), { Cameroon | Cameroun }
   {041}(ISO3166N: '124'; ISO31663: 'CAN'; ISO31662: 'CA'), { Canada | Canada }
   {042}(ISO3166N: '132'; ISO31663: 'CPV'; ISO31662: 'CV'), { Cabo Verde | Cap-Vert }
   {043}(ISO3166N: '140'; ISO31663: 'CAF'; ISO31662: 'CF'), { Central African Republic | République centreafricaine }
   {044}(ISO3166N: '152'; ISO31663: 'CHL'; ISO31662: 'CL'), { Chile | Chili }
   {045}(ISO3166N: '156'; ISO31663: 'CHN'; ISO31662: 'CN'), { China | Chine }
   {046}(ISO3166N: '162'; ISO31663: 'CXR'; ISO31662: 'CX'), { Christmas Island | Iles Christmas }
   {047}(ISO3166N: '196'; ISO31663: 'CYP'; ISO31662: 'CY'), { Cyprus | Chypre }
   {048}(ISO3166N: '166'; ISO31663: 'CCK'; ISO31662: 'CC'), { Cocos (Keeling) Islands | Iles Cocos }
   {049}(ISO3166N: '170'; ISO31663: 'COL'; ISO31662: 'CO'), { Colombia | Colombie }
   {050}(ISO3166N: '174'; ISO31663: 'COM'; ISO31662: 'KM'), { Comoros | Comores }
   {051}(ISO3166N: '178'; ISO31663: 'COG'; ISO31662: 'CG'), { Congo | République du Congo }
   {052}(ISO3166N: '180'; ISO31663: 'COD'; ISO31662: 'CD'), { Congo (Democratic Republic of the) | République démocratique du Congo }
   {053}(ISO3166N: '184'; ISO31663: 'COK'; ISO31662: 'CK'), { Cook Islands | Iles Cook }
   {054}(ISO3166N: '410'; ISO31663: 'KOR'; ISO31662: 'KR'), { Korea (Republic of) | Corée du Sud }
   {055}(ISO3166N: '408'; ISO31663: 'PRK'; ISO31662: 'KP'), { Korea (Democratic People's Republic of) | Corée du Nord }
   {056}(ISO3166N: '188'; ISO31663: 'CRI'; ISO31662: 'CR'), { Costa Rica | Costa Rica }
   {057}(ISO3166N: '384'; ISO31663: 'CIV'; ISO31662: 'CI'), { Côte d'Ivoire | Côte d'Ivoire }
   {058}(ISO3166N: '191'; ISO31663: 'HRV'; ISO31662: 'HR'), { Croatia | Croatie }
   {059}(ISO3166N: '192'; ISO31663: 'CUB'; ISO31662: 'CU'), { Cuba | Cuba }
   {060}(ISO3166N: '531'; ISO31663: 'CUW'; ISO31662: 'CW'), { Curaçao | Curaçao }
   {061}(ISO3166N: '208'; ISO31663: 'DNK'; ISO31662: 'DK'), { Denmark | Danemark }
   {062}(ISO3166N: '262'; ISO31663: 'DJI'; ISO31662: 'DJ'), { Djibouti | Djibouti }
   {063}(ISO3166N: '214'; ISO31663: 'DOM'; ISO31662: 'DO'), { Dominican Republic | République dominicaine }
   {064}(ISO3166N: '212'; ISO31663: 'DMA'; ISO31662: 'DM'), { Dominica | Dominique }
   {065}(ISO3166N: '818'; ISO31663: 'EGY'; ISO31662: 'EG'), { Egypt | Egypte }
   {066}(ISO3166N: '222'; ISO31663: 'SLV'; ISO31662: 'SV'), { El Salvador | Salvador }
   {067}(ISO3166N: '784'; ISO31663: 'ARE'; ISO31662: 'AE'), { United Arab Emirates | Emirats arabes unis }
   {068}(ISO3166N: '218'; ISO31663: 'ECU'; ISO31662: 'EC'), { Ecuador | Equateur }
   {069}(ISO3166N: '232'; ISO31663: 'ERI'; ISO31662: 'ER'), { Eritrea | Erythrée }
   {070}(ISO3166N: '724'; ISO31663: 'ESP'; ISO31662: 'ES'), { Spain | Espagne }
   {071}(ISO3166N: '233'; ISO31663: 'EST'; ISO31662: 'EE'), { Estonia | Estonie }
   {072}(ISO3166N: '840'; ISO31663: 'USA'; ISO31662: 'US'), { United States of America | Etats-Unis }
   {073}(ISO3166N: '231'; ISO31663: 'ETH'; ISO31662: 'ET'), { Ethiopia | Ethiopie }
   {074}(ISO3166N: '238'; ISO31663: 'FLK'; ISO31662: 'FK'), { Falkland Islances (Malvinas) | Malouines }
   {075}(ISO3166N: '234'; ISO31663: 'FRO'; ISO31662: 'FO'), { Faroe Islands | Iles Féroé }
   {076}(ISO3166N: '242'; ISO31663: 'FJI'; ISO31662: 'FJ'), { Fiji | Fidji }
   {077}(ISO3166N: '246'; ISO31663: 'FIN'; ISO31662: 'FI'), { Finland | Finlande }
   {078}(ISO3166N: '250'; ISO31663: 'FRA'; ISO31662: 'FR'), { France | France }
   {079}(ISO3166N: '266'; ISO31663: 'GAB'; ISO31662: 'GA'), { Gabon | Gabon }
   {080}(ISO3166N: '270'; ISO31663: 'GMB'; ISO31662: 'GM'), { Gambia | Gambie }
   {081}(ISO3166N: '268'; ISO31663: 'GEO'; ISO31662: 'GE'), { Georgia | Géorgie }
   {082}(ISO3166N: '239'; ISO31663: 'SGS'; ISO31662: 'GS'), { South Georgia and the South Sandwich Islands | Géorgie du Sud-et-Les Iles Sandwich du Sud }
   {083}(ISO3166N: '288'; ISO31663: 'GHA'; ISO31662: 'GH'), { Ghana | Ghana }
   {084}(ISO3166N: '292'; ISO31663: 'GIB'; ISO31662: 'GI'), { Gibraltar | Gibraltar }
   {085}(ISO3166N: '300'; ISO31663: 'GRC'; ISO31662: 'GR'), { Greece | Grèce }
   {086}(ISO3166N: '308'; ISO31663: 'GRD'; ISO31662: 'GD'), { Grenada | Grenade (pays) }
   {087}(ISO3166N: '304'; ISO31663: 'GRL'; ISO31662: 'GL'), { Greenland | Groenland }
   {088}(ISO3166N: '312'; ISO31663: 'GLP'; ISO31662: 'GP'), { Guadeloupe | Guadeloupe }
   {089}(ISO3166N: '316'; ISO31663: 'GUM'; ISO31662: 'GU'), { Guam | Guam }
   {090}(ISO3166N: '320'; ISO31663: 'GTM'; ISO31662: 'GT'), { Guatemala | Guatemala }
   {091}(ISO3166N: '831'; ISO31663: 'GGY'; ISO31662: 'GG'), { Guernsey | Guernesey }
   {092}(ISO3166N: '324'; ISO31663: 'GIN'; ISO31662: 'GN'), { Guinea | Guinée }
   {093}(ISO3166N: '624'; ISO31663: 'GNB'; ISO31662: 'GW'), { Guinea-Bissau | Guinée-Bissau }
   {094}(ISO3166N: '226'; ISO31663: 'GNQ'; ISO31662: 'GQ'), { Equatorial Guinea | Guinée équatoriale }
   {095}(ISO3166N: '328'; ISO31663: 'GUY'; ISO31662: 'GY'), { Guyana | Guyana }
   {096}(ISO3166N: '254'; ISO31663: 'GUF'; ISO31662: 'GF'), { Fench Guiana | Guyane }
   {097}(ISO3166N: '332'; ISO31663: 'HTI'; ISO31662: 'HT'), { Haiti | Haïti }
   {098}(ISO3166N: '334'; ISO31663: 'HMD'; ISO31662: 'HM'), { Heard Island and McDonald Islandds | Iles Heard-et-MacDonald }
   {099}(ISO3166N: '340'; ISO31663: 'HND'; ISO31662: 'HN'), { Honduras | Honduras }
   {100}(ISO3166N: '344'; ISO31663: 'HKG'; ISO31662: 'HK'), { Hong Kong | Hong Kong }
   {101}(ISO3166N: '348'; ISO31663: 'HUN'; ISO31662: 'HU'), { Hungary | Hongrie }
   {102}(ISO3166N: '833'; ISO31663: 'IMN'; ISO31662: 'IM'), { Isle of Man | Ile de Man }
   {103}(ISO3166N: '581'; ISO31663: 'UMI'; ISO31662: 'UM'), { United States Minor Outlying Islands | Iles mineures éloignées des Etats-Unis }
   {104}(ISO3166N: '092'; ISO31663: 'VGB'; ISO31662: 'VG'), { Virgin Islands (British) | Iles Vierges britanniques }
   {105}(ISO3166N: '850'; ISO31663: 'VIR'; ISO31662: 'VI'), { Virgin Islands (U.S.) | Iles Vierges des Etats-Unis }
   {106}(ISO3166N: '356'; ISO31663: 'IND'; ISO31662: 'IN'), { India | Inde }
   {107}(ISO3166N: '360'; ISO31663: 'IDN'; ISO31662: 'ID'), { Indonesia | Indonésie }
   {108}(ISO3166N: '364'; ISO31663: 'IRN'; ISO31662: 'IR'), { Iran (Islamic Republic of) | Iran }
   {109}(ISO3166N: '368'; ISO31663: 'IRQ'; ISO31662: 'IQ'), { Iraq | Irak }
   {110}(ISO3166N: '372'; ISO31663: 'IRL'; ISO31662: 'IE'), { Ireland | Irlande }
   {111}(ISO3166N: '352'; ISO31663: 'ISL'; ISO31662: 'IS'), { Iceland | Islande }
   {112}(ISO3166N: '376'; ISO31663: 'ISR'; ISO31662: 'IL'), { Israel | Israël }
   {113}(ISO3166N: '380'; ISO31663: 'ITA'; ISO31662: 'IT'), { Italy | Italie }
   {114}(ISO3166N: '388'; ISO31663: 'JAM'; ISO31662: 'JM'), { Jamaica | Jamaïque }
   {115}(ISO3166N: '392'; ISO31663: 'JPN'; ISO31662: 'JP'), { Japan | Japon }
   {116}(ISO3166N: '832'; ISO31663: 'JEY'; ISO31662: 'JE'), { Jersey | Jersey }
   {117}(ISO3166N: '400'; ISO31663: 'JOR'; ISO31662: 'JO'), { Jordan | Jordanie }
   {118}(ISO3166N: '398'; ISO31663: 'KAZ'; ISO31662: 'KZ'), { Kazakhstan | Kazakhstan }
   {119}(ISO3166N: '404'; ISO31663: 'KEN'; ISO31662: 'KE'), { Kenya | Kenya }
   {120}(ISO3166N: '417'; ISO31663: 'KGZ'; ISO31662: 'KG'), { Kyrgyzstan | Kirghizistan }
   {121}(ISO3166N: '296'; ISO31663: 'KIR'; ISO31662: 'KI'), { Kiribati | Kiribati }
   {122}(ISO3166N: '414'; ISO31663: 'KWT'; ISO31662: 'KW'), { Kuwait | Koweit }
   {123}(ISO3166N: '418'; ISO31663: 'LAO'; ISO31662: 'LA'), { Lao People's Democratic Republic | Laos }
   {124}(ISO3166N: '426'; ISO31663: 'LSO'; ISO31662: 'LS'), { Lesotho | Lesotho }
   {125}(ISO3166N: '428'; ISO31663: 'LVA'; ISO31662: 'LV'), { Latvia | Lettonie }
   {126}(ISO3166N: '422'; ISO31663: 'LBN'; ISO31662: 'LB'), { Lebanon | Liban }
   {127}(ISO3166N: '430'; ISO31663: 'LBR'; ISO31662: 'LR'), { Liberia | Liberia }
   {128}(ISO3166N: '434'; ISO31663: 'LBY'; ISO31662: 'LY'), { Libya | Libye }
   {129}(ISO3166N: '438'; ISO31663: 'LIE'; ISO31662: 'LI'), { Liechtenstein | Liechtenstein }
   {130}(ISO3166N: '440'; ISO31663: 'LTU'; ISO31662: 'LT'), { Lithuania | Lituanie }
   {131}(ISO3166N: '442'; ISO31663: 'LUX'; ISO31662: 'LU'), { Luxembourg | Luxembourg }
   {132}(ISO3166N: '446'; ISO31663: 'MAC'; ISO31662: 'MO'), { Macao | Macao }
   {133}(ISO3166N: '807'; ISO31663: 'MKD'; ISO31662: 'MK'), { Macedonia (the former Yugoslav Republic of) | République de Macédoine (pays) }
   {134}(ISO3166N: '450'; ISO31663: 'MDG'; ISO31662: 'MG'), { Madagascar | Madagascar }
   {135}(ISO3166N: '458'; ISO31663: 'MYS'; ISO31662: 'MY'), { Malaysia | Malaisie }
   {136}(ISO3166N: '454'; ISO31663: 'MWI'; ISO31662: 'MW'), { Malawi | Malawi }
   {137}(ISO3166N: '462'; ISO31663: 'MDV'; ISO31662: 'MV'), { Maldives | Maldives }
   {138}(ISO3166N: '466'; ISO31663: 'MLI'; ISO31662: 'ML'), { Mali | Mali }
   {139}(ISO3166N: '470'; ISO31663: 'MLT'; ISO31662: 'MT'), { Malta | Malte }
   {140}(ISO3166N: '580'; ISO31663: 'MNP'; ISO31662: 'MP'), { Northern Mariana Islands | Iles Mariannes du Nord }
   {141}(ISO3166N: '504'; ISO31663: 'MAR'; ISO31662: 'MA'), { Morocco | Maroc }
   {142}(ISO3166N: '584'; ISO31663: 'MHL'; ISO31662: 'MH'), { Marshall Islands | Iles Marshall }
   {143}(ISO3166N: '474'; ISO31663: 'MTQ'; ISO31662: 'MQ'), { Martinique | Martinique }
   {144}(ISO3166N: '480'; ISO31663: 'MUS'; ISO31662: 'MU'), { Mauritius | Maurice }
   {145}(ISO3166N: '478'; ISO31663: 'MRT'; ISO31662: 'MR'), { Mauritania | Mauritanie }
   {146}(ISO3166N: '175'; ISO31663: 'MYT'; ISO31662: 'YT'), { Mayotte | Mayotte }
   {147}(ISO3166N: '484'; ISO31663: 'MEX'; ISO31662: 'MX'), { Mexico | Mexique }
   {148}(ISO3166N: '583'; ISO31663: 'FSM'; ISO31662: 'FM'), { Micronesia (Federated States of) | Micronésie (pays) }
   {149}(ISO3166N: '498'; ISO31663: 'MDA'; ISO31662: 'MD'), { Moldova (Republic of) | Moldavie }
   {150}(ISO3166N: '492'; ISO31663: 'MCO'; ISO31662: 'MC'), { Monaco | Monaco }
   {151}(ISO3166N: '496'; ISO31663: 'MNG'; ISO31662: 'MN'), { Mongolia | Mongolie }
   {152}(ISO3166N: '499'; ISO31663: 'MNE'; ISO31662: 'ME'), { Montenegro | Monténégro }
   {153}(ISO3166N: '500'; ISO31663: 'MSR'; ISO31662: 'MS'), { Montserrat | Montserrat }
   {154}(ISO3166N: '508'; ISO31663: 'MOZ'; ISO31662: 'MZ'), { Mozambique | Mozambique }
   {155}(ISO3166N: '104'; ISO31663: 'MMR'; ISO31662: 'MM'), { Myanmar | Birmanie }
   {156}(ISO3166N: '516'; ISO31663: 'NAM'; ISO31662: 'NA'), { Namibia | Namibie }
   {157}(ISO3166N: '520'; ISO31663: 'NRU'; ISO31662: 'NR'), { Nauru | Nauru }
   {158}(ISO3166N: '524'; ISO31663: 'NPL'; ISO31662: 'NP'), { Nepal | Népal }
   {159}(ISO3166N: '558'; ISO31663: 'NIC'; ISO31662: 'NI'), { Nicaragua | Nicaragua }
   {160}(ISO3166N: '562'; ISO31663: 'NER'; ISO31662: 'NE'), { Niger | Niger }
   {161}(ISO3166N: '566'; ISO31663: 'NGA'; ISO31662: 'NG'), { Nigeria | Nigeria }
   {162}(ISO3166N: '570'; ISO31663: 'NIU'; ISO31662: 'NU'), { Niue | Niue }
   {163}(ISO3166N: '574'; ISO31663: 'NFK'; ISO31662: 'NF'), { Norfolk Island | Ile Norfolk }
   {164}(ISO3166N: '578'; ISO31663: 'NOR'; ISO31662: 'NO'), { Norway | Norvège }
   {165}(ISO3166N: '540'; ISO31663: 'NCL'; ISO31662: 'NC'), { New Caledonia | Nouvelle-Calédonie }
   {166}(ISO3166N: '554'; ISO31663: 'NZL'; ISO31662: 'NZ'), { New Zealand | Nouvelle-Zélande }
   {167}(ISO3166N: '086'; ISO31663: 'IOT'; ISO31662: 'IO'), { British Indian Ocean Territory | Territoire britannique de l'océan Indien }
   {168}(ISO3166N: '512'; ISO31663: 'OMN'; ISO31662: 'OM'), { Oman | Oman }
   {169}(ISO3166N: '800'; ISO31663: 'UGA'; ISO31662: 'UG'), { Uganda | Ouganda }
   {170}(ISO3166N: '860'; ISO31663: 'UZB'; ISO31662: 'UZ'), { Uzbekistan | Ouzbékistan }
   {171}(ISO3166N: '586'; ISO31663: 'PAK'; ISO31662: 'PK'), { Pakistan | Pakistan }
   {172}(ISO3166N: '585'; ISO31663: 'PLW'; ISO31662: 'PW'), { Palau | Palaos }
   {173}(ISO3166N: '275'; ISO31663: 'PSE'; ISO31662: 'PS'), { Palestine, State of | Palestine }
   {174}(ISO3166N: '591'; ISO31663: 'PAN'; ISO31662: 'PA'), { Panama | Panama }
   {175}(ISO3166N: '598'; ISO31663: 'PNG'; ISO31662: 'PG'), { Papua New Guinea | Papouasie-Nouvelle-Guinée }
   {176}(ISO3166N: '600'; ISO31663: 'PRY'; ISO31662: 'PY'), { Paraguay | Paraguay }
   {177}(ISO3166N: '528'; ISO31663: 'NLD'; ISO31662: 'NL'), { Netherlands | Pays-Bas }
   {178}(ISO3166N: '604'; ISO31663: 'PER'; ISO31662: 'PE'), { Peru | Pérou }
   {179}(ISO3166N: '608'; ISO31663: 'PHL'; ISO31662: 'PH'), { Philippines | Philippines }
   {180}(ISO3166N: '612'; ISO31663: 'PCN'; ISO31662: 'PN'), { Pitcairn | Iles Pitcairn }
   {181}(ISO3166N: '616'; ISO31663: 'POL'; ISO31662: 'PL'), { Poland | Pologne }
   {182}(ISO3166N: '258'; ISO31663: 'PYF'; ISO31662: 'PF'), { French Polynesia | Polynésie française }
   {183}(ISO3166N: '630'; ISO31663: 'PRI'; ISO31662: 'PR'), { Puerto Rico | Porto Rico }
   {184}(ISO3166N: '620'; ISO31663: 'PRT'; ISO31662: 'PT'), { Portugal | Portugal }
   {185}(ISO3166N: '634'; ISO31663: 'QAT'; ISO31662: 'QA'), { Qatar | Qatar }
   {186}(ISO3166N: '638'; ISO31663: 'REU'; ISO31662: 'RE'), { Réunion | La Réunion }
   {187}(ISO3166N: '642'; ISO31663: 'ROU'; ISO31662: 'RO'), { Romania | Roumanie }
   {188}(ISO3166N: '826'; ISO31663: 'GBR'; ISO31662: 'GB'), { United Kingdom of Great Britain and Northern Ireland | Royaume-Uni }
   {189}(ISO3166N: '643'; ISO31663: 'RUS'; ISO31662: 'RU'), { Russian Federation | Russie }
   {190}(ISO3166N: '646'; ISO31663: 'RWA'; ISO31662: 'RW'), { Rwanda | Rwanda }
   {191}(ISO3166N: '732'; ISO31663: 'ESH'; ISO31662: 'EH'), { Western Sahara | République arabae sahraouie démocratique }
   {192}(ISO3166N: '652'; ISO31663: 'BLM'; ISO31662: 'BL'), { Saint Barthélemy | Saint-Barthélemy }
   {193}(ISO3166N: '659'; ISO31663: 'KNA'; ISO31662: 'KN'), { Saint Kitts and Nevis | Saint-Christophe-et-Niévès }
   {194}(ISO3166N: '674'; ISO31663: 'SMR'; ISO31662: 'SM'), { San Marino | Saint-Marin }
   {195}(ISO3166N: '663'; ISO31663: 'MAF'; ISO31662: 'MF'), { Saint Martin (French part) | Saint-Martin }
   {197}(ISO3166N: '534'; ISO31663: 'SXM'; ISO31662: 'SX'), { Sint Maarten (Dutch part) | Saint-Martin }
   {198}(ISO3166N: '666'; ISO31663: 'SPM'; ISO31662: 'PM'), { Saint Pierre and Miquelon | Saint-Pierre-et-Miquelon }
   {199}(ISO3166N: '336'; ISO31663: 'VAT'; ISO31662: 'VA'), { Holy See | Saint-Siège (Etat de cla Cité du Vatican) }
   {200}(ISO3166N: '670'; ISO31663: 'VCT'; ISO31662: 'VC'), { Saint Vincent and the Grenadines | Saint-Vincent-et-les Grenadines }
   {201}(ISO3166N: '654'; ISO31663: 'SHN'; ISO31662: 'SH'), { Saint Helena, Ascension and Tristan da Cunha | Sainte-Hélène, Ascension et Tristan da Cunha }
   {202}(ISO3166N: '662'; ISO31663: 'LCA'; ISO31662: 'LC'), { Saint Lucia | Sainte-Lucie }
   {203}(ISO3166N: '090'; ISO31663: 'SLB'; ISO31662: 'SB'), { Solomon Islands | Salomon }
   {204}(ISO3166N: '882'; ISO31663: 'WSM'; ISO31662: 'WS'), { Samoa | Samoa }
   {205}(ISO3166N: '016'; ISO31663: 'ASM'; ISO31662: 'AS'), { American Samoa | Samoa américaines }
   {206}(ISO3166N: '678'; ISO31663: 'STP'; ISO31662: 'ST'), { Sao Tome and Principe | Sao Tomé-et-Principe }
   {207}(ISO3166N: '686'; ISO31663: 'SEN'; ISO31662: 'SN'), { Senegal | Sénégal }
   {208}(ISO3166N: '688'; ISO31663: 'SRB'; ISO31662: 'RS'), { Serbia | Serbie }
   {209}(ISO3166N: '690'; ISO31663: 'SYC'; ISO31662: 'SC'), { Seychelles | Seychelles }
   {210}(ISO3166N: '694'; ISO31663: 'SLE'; ISO31662: 'SL'), { Sierra Leone | Sierra Leone }
   {211}(ISO3166N: '702'; ISO31663: 'SGP'; ISO31662: 'SG'), { Singapore | Singapour }
   {212}(ISO3166N: '703'; ISO31663: 'SVK'; ISO31662: 'SK'), { Slovakia | Slovaquie }
   {213}(ISO3166N: '705'; ISO31663: 'SVN'; ISO31662: 'SI'), { Slovenia | Slovénie }
   {214}(ISO3166N: '706'; ISO31663: 'SOM'; ISO31662: 'SO'), { Somalia | Somalie }
   {215}(ISO3166N: '729'; ISO31663: 'SDN'; ISO31662: 'SD'), { Sudan | Soudan }
   {216}(ISO3166N: '728'; ISO31663: 'SSD'; ISO31662: 'SS'), { South Sudan | Soudan du Sud }
   {217}(ISO3166N: '144'; ISO31663: 'LKA'; ISO31662: 'LK'), { Sri Lanka | Sri Lanka }
   {218}(ISO3166N: '752'; ISO31663: 'SWE'; ISO31662: 'SE'), { Sweden | Suède }
   {219}(ISO3166N: '756'; ISO31663: 'CHE'; ISO31662: 'CH'), { Switzerland | Suisse }
   {220}(ISO3166N: '740'; ISO31663: 'SUR'; ISO31662: 'SR'), { Suriname | Suriname }
   {221}(ISO3166N: '744'; ISO31663: 'SJM'; ISO31662: 'SJ'), { Svalbard and Jan Mayen | Svalbard et ile Jan Mayen }
   {222}(ISO3166N: '748'; ISO31663: 'SWZ'; ISO31662: 'SZ'), { Swaziland | Swaziland }
   {223}(ISO3166N: '760'; ISO31663: 'SYR'; ISO31662: 'SZ'), { Syrian Arab Republic | Syrie }
   {224}(ISO3166N: '762'; ISO31663: 'TJK'; ISO31662: 'TJ'), { Tajikistan | Tadjikistan }
   {225}(ISO3166N: '158'; ISO31663: 'TWN'; ISO31662: 'TW'), { Taiwan, Province of China | Taïwan }
   {226}(ISO3166N: '834'; ISO31663: 'TZA'; ISO31662: 'TZ'), { Tanzania, United Republic of | Tanzanie }
   {227}(ISO3166N: '148'; ISO31663: 'TCD'; ISO31662: 'TD'), { Chad | Tchad }
   {228}(ISO3166N: '203'; ISO31663: 'CZE'; ISO31662: 'CZ'), { Czechia | Tchéquie }
   {229}(ISO3166N: '260'; ISO31663: 'ATF'; ISO31662: 'TF'), { French Southern Territories | Terres australes et antartiques françaises }
   {230}(ISO3166N: '764'; ISO31663: 'THA'; ISO31662: 'TH'), { Thailand | Thaïlande }
   {231}(ISO3166N: '626'; ISO31663: 'TLS'; ISO31662: 'TL'), { Timor-Leste | Timor oriental }
   {232}(ISO3166N: '768'; ISO31663: 'TGO'; ISO31662: 'TG'), { Togo | Togo }
   {233}(ISO3166N: '772'; ISO31663: 'TKL'; ISO31662: 'TK'), { Tokelau | Tokelau }
   {234}(ISO3166N: '776'; ISO31663: 'TON'; ISO31662: 'TO'), { Tonga | Tonga }
   {235}(ISO3166N: '780'; ISO31663: 'TTO'; ISO31662: 'TT'), { Trinidad and Tobago | Trinité-et-Tobago }
   {236}(ISO3166N: '788'; ISO31663: 'TUN'; ISO31662: 'TN'), { Tunisia | Tunisie }
   {237}(ISO3166N: '795'; ISO31663: 'TKM'; ISO31662: 'TM'), { Turkmenistan | Turkménistan }
   {238}(ISO3166N: '796'; ISO31663: 'TCA'; ISO31662: 'TC'), { Turks and Caicos Islands | Iles Turques-et-Caïques }
   {239}(ISO3166N: '792'; ISO31663: 'TUR'; ISO31662: 'TR'), { Turkey | Turquie }
   {240}(ISO3166N: '798'; ISO31663: 'TUV'; ISO31662: 'TV'), { Tuvalu | Tuvalu }
   {241}(ISO3166N: '804'; ISO31663: 'UKR'; ISO31662: 'UA'), { Ukraine | Ukraine }
   {242}(ISO3166N: '858'; ISO31663: 'URY'; ISO31662: 'UA'), { Uruguay | Uruguay }
   {243}(ISO3166N: '548'; ISO31663: 'VUT'; ISO31662: 'VU'), { Vanuatu | Vanuatu }
   {244}(ISO3166N: '862'; ISO31663: 'VEN'; ISO31662: 'VE'), { Venezuela (Bolivarian Republic of) | Venezuela }
   {245}(ISO3166N: '704'; ISO31663: 'VNM'; ISO31662: 'VN'), { Viet Nam | Viêt Nam }
   {246}(ISO3166N: '876'; ISO31663: 'WLF'; ISO31662: 'WF'), { Wallis and Futuna | Wallis-et-Futuna }
   {247}(ISO3166N: '887'; ISO31663: 'YEM'; ISO31662: 'YE'), { Yemen | Yemen }
   {248}(ISO3166N: '894'; ISO31663: 'ZMB'; ISO31662: 'ZM'), { Zambia | Zambie }
   {249}(ISO3166N: '716'; ISO31663: 'ZWE'; ISO31662: 'ZW')  { Zimbabwe | Zimbabwe }
  );

function ISO31663To2(aISO31663: TISO31663): TISO31662;
function ISO31663ToN(aISO31663: TISO31663): TISO3166N;
function ISO31662To3(aISO31662: TISO31662): TISO31663;
function ISO3166NTo3(aISO3166N: TISO3166N): TISO31663;

procedure LoadISO31663(var aStrings: TStringList);

implementation

function ISO31663To2(aISO31663: TISO31663): TISO31662;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    if StatesCodesTable[iItem].ISO31663 = aISO31663 then
    begin
      Result:= StatesCodesTable[iItem].ISO31662;
      break;
    end;
end;

function ISO31663ToN(aISO31663: TISO31663): TISO3166N;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    if StatesCodesTable[iItem].ISO31663 = aISO31663 then
    begin
      Result:= StatesCodesTable[iItem].ISO3166N;
      break;
    end;
end;

function ISO31662To3(aISO31662: TISO31662): TISO31663;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    if StatesCodesTable[iItem].ISO31662 = aISO31662 then
    begin
      Result:= StatesCodesTable[iItem].ISO31663;
      break;
    end;
end;

function ISO3166NTo3(aISO3166N: TISO3166N): TISO31663;
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    if StatesCodesTable[iItem].ISO3166N = aISO3166N then
    begin
      Result:= StatesCodesTable[iItem].ISO31663;
      break;
    end;
end;

procedure LoadISO31663(var aStrings: TStringList);
var
  iItem: integer;
begin
  for iItem:= 0 to Length(StatesCodesTable) - 1 do
    aStrings.Add(StatesCodesTable[iItem].ISO31663);
end;

end.

