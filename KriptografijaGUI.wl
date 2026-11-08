(* ::Package:: *)

(* Graficki interfejs za paket Kriptografija.wl *)
(* Magistarski rad: Implementacija softverskog paketa Wolfram Mathematica u kriptografiji javnog kljuca *)
(* :Author: Mirnes Smajilovic *)
(* :Context: KriptografijaGUI` *)
(* :Package Version: 1.0 *)
(* :Discussion: Interfejs ne mijenja paket Kriptografija.wl. Sve algoritme izvrsavaju funkcije iz tog paketa. *)
(* Pokretanje: Get["KriptografijaGUI.wl"]; KriptografijaGUI[] *)


(* Ako paket Kriptografija` nije ucitan, ucitava se iz iste mape u kojoj je ova datoteka. *)
Module[{mapa},
  If[Length[Names["Kriptografija`MillerRabinTest"]] == 0,
    mapa = If[$InputFileName =!= "", DirectoryName[$InputFileName],
      Quiet[Check[NotebookDirectory[], Directory[]]]];
    Get[FileNameJoin[{mapa, "Kriptografija.wl"}]]
  ]
]

BeginPackage["KriptografijaGUI`", {"Kriptografija`"}]

KriptografijaGUI::usage = "KriptografijaGUI[] otvara grafi\[CHacek]ki interfejs (novi dokument) za pokretanje algoritama iz paketa Kriptografija.wl: ulazni podaci se unose u polja, a izlaz se prikazuje tabelarno i grafi\[CHacek]ki.";

Begin["`Private`"]


(* ------------------------------------------------------------------ *)
(* Pomocne funkcije                                                   *)
(* ------------------------------------------------------------------ *)

cijeliQ[l_List] := VectorQ[l, IntegerQ]

greska[s_String] := Style[zag[s], Red, Bold]

neCijeli = greska["Svi ulazni podaci moraju biti cijeli brojevi."];

$ogranicenje = 60;

(* Objasnjenja uz slike i tabele: indeksi i eksponenti se prikazuju pravim oznakama (zag),
   a tekst se lomi u redove jer Row ne prelama redove. *)
pasus[tekst_String, vel_: 11, boja_: GrayLevel[0.4], sirina_: 110] := Module[{rijeci, redovi = {}, tren = ""},
  rijeci = StringSplit[tekst, " "];
  Do[
    If[tren === "", tren = r,
      If[StringLength[tren] + StringLength[r] + 1 > sirina, AppendTo[redovi, tren]; tren = r, tren = tren <> " " <> r]],
    {r, rijeci}];
  If[tren =!= "", AppendTo[redovi, tren]];
  Column[Style[zag[#], vel, boja] & /@ redovi, Spacings -> 0.2]
]
pasus[x_, vel_: 11, boja_: GrayLevel[0.4], sirina_: 110] := Style[x, vel, boja]

objasni[x_] := pasus[x, 11, Gray]


(* Dijagnosticki rezim (zaglavlje prozora): kad je ukljucen, ne koristi se Quiet,
   pa se poruke Mathematice vide u biljeznici, a ispisuju se i uz gresku. *)
$dijagnostika = False;

porukeMath[prije_Integer] := Module[{nove},
  nove = Drop[$MessageList, Min[Max[prije, 0], Length[$MessageList]]];
  If[nove === {},
    objasni["Mathematica nije prijavila poruke."],
    Column[Prepend[Style[ToString[#, InputForm], FontFamily -> "Courier", 10] & /@ nove,
      Style["Poruke Mathematice (dijagnostika):", Bold, 11]]]]
]

(* izvrsavanje s prikazom ishoda: uspjeh, prekoracenje vremena, neuspjeh izracunavanja *)
SetAttributes[pokreni, HoldFirst]
pokreni[izraz_, lim_] :=
  If[TrueQ[$dijagnostika],
    TimeConstrained[Check[izraz, $Failed], lim, $Aborted],
    TimeConstrained[Quiet[Check[izraz, $Failed]], lim, $Aborted]]

SetAttributes[izlaz, HoldFirst]
izlaz[izraz_, ___] := Module[{r, lim, prije = Length[$MessageList]},
  lim = If[NumberQ[$ogranicenje] && $ogranicenje > 0, $ogranicenje, Infinity];
  r = pokreni[izraz, lim];
  Which[
    r === $Aborted, greska["Izra\[CHacek]un je prekinut: prekora\[CHacek]eno je zadano vremensko ograni\[CHacek]enje (" <> ToString[lim] <> " s). Pove\[CAcute]ajte ograni\[CHacek]enje ili upi\[SHacek]ite 0 (bez ograni\[CHacek]enja)."],
    r === $Failed, Column[{greska["Izra\[CHacek]un nije uspio. Provjerite ulazne podatke."],
      If[TrueQ[$dijagnostika], porukeMath[prije], Nothing]}],
    True, r
  ]
]

(* varijanta bez zajednickog ogranicenja: ogranicenje se primjenjuje na svaku metodu posebno *)
SetAttributes[izlazBez, HoldFirst]
izlazBez[izraz_, ___] := Module[{r, prije = Length[$MessageList]},
  r = If[TrueQ[$dijagnostika], Check[izraz, $Failed], Quiet[Check[izraz, $Failed]]];
  If[r === $Failed,
    Column[{greska["Izra\[CHacek]un nije uspio. Provjerite ulazne podatke."],
      If[TrueQ[$dijagnostika], porukeMath[prije], Nothing]}],
    r]
]


(* ------------------------------------------------------------------ *)
(* Stanje BBS generatora (GUI vodi vlastiti indikator)                 *)
(* Simboli BBSModul i BBSStanje su privatni u paketu, pa ih GUI ne    *)
(* cita. Indikator prikazuje stanje kojim upravlja GUI i ne garantuje *)
(* interno stanje paketa ako se BBS pokrene izvan GUI-ja.             *)
(* ------------------------------------------------------------------ *)

$bbsStatus = None;
$bbsPoruka = "";

vrijemeSada[] := DateString[{"Hour", ":", "Minute", ":", "Second"}]

pokreniBBS[m_Integer] := (BBSPokreni[m];
  $bbsStatus = "m = " <> ToString[m] <> " bita, " <> vrijemeSada[];
  $bbsPoruka = "";)

(* funkcije koje trebaju BBS: pokrece se samo ako generator nije pokrenut iz GUI-ja *)
osiguraBBS[m_Integer : 32] := If[$bbsStatus === None,
  pokreniBBS[m];
  $bbsPoruka = "BBS je automatski pokrenut (m = " <> ToString[m] <> ") jer nije bio pokrenut."]

(* funkcije paketa Generisi... same pozivaju BBSPokreni: stanje se tada mijenja *)
bbsPromijenjen[ime_String] := (
  $bbsStatus = "ponovo pokrenut unutar funkcije " <> ime <> ", " <> vrijemeSada[];
  $bbsPoruka = "")

bbsIndikator[] := DynamicModule[{mB = 32},
  Column[{
    Row[{Style["BBS generator: ", Bold],
      Dynamic[If[$bbsStatus === None,
        Style["nije pokrenut", Red, Bold],
        Style["pokrenut iz GUI-ja (" <> $bbsStatus <> ")", Darker[Green], Bold]]],
      Spacer[15], Style["m = ", Bold], InputField[Dynamic[mB, Function[val, mB = cijeloIzUnosa[val]]], Expression, FieldSize -> 5], Spacer[6],
      Button["Pokreni BBS (m)", If[IntegerQ[mB] && mB >= 5, pokreniBBS[mB], $bbsPoruka = "Potrebno je m \[GreaterEqual] 5."],
        Method -> "Queued", ImageSize -> Automatic]}],
    Dynamic[If[$bbsPoruka === "", "", Style[$bbsPoruka, 11, Darker[Orange]]]],
    Style["Indikator prikazuje stanje kojim upravlja GUI; ako se BBS pokrene izvan GUI-ja, ne mora biti ta\[CHacek]an.", 10, Gray]
  }, Spacings -> 0.3]
]

slucajnaNapomena := objasni["Rezultat zavisi od slu\[CHacek]ajnog broja iz BBS generatora; ponovno izvr\[SHacek]avanje ne mora dati iste vrijednosti."];

(* oznake JAVNO / TAJNO *)
jt[opis_, "JAVNO"] := Row[{zag[opis], Spacer[6], Style[" JAVNO ", 9, Bold, White, Background -> Darker[Green]]}]
jt[opis_, "TAJNO"] := Row[{zag[opis], Spacer[6], Style[" TAJNO ", 9, Bold, White, Background -> Darker[Red]]}]

jtLegenda := objasni["JAVNO: smije se objaviti. TAJNO: ne smije se objaviti (ovdje se prikazuje radi u\[CHacek]enja)."];

$prekoraceno = "Prekora\[CHacek]eno vrijeme.";

(* mjeri se samo poziv funkcije iz paketa (bez tabela, grafikona i formatiranja) *)
izmjeri[ime_String, n_Integer, par_Association, ogr_, ponavljanja_Integer] := Module[{lim, prvi = None, t, r, redovi = {}},
  lim = If[ogr > 0, ogr, Infinity];
  Do[
    {t, r} = AbsoluteTiming[
      TimeConstrained[Quiet[Check[pokreniMetodu[ime, n, par], "Gre\[SHacek]ka pri izvr\[SHacek]avanju."]], lim, $prekoraceno]];
    If[prvi === None, prvi = r];
    AppendTo[redovi, {t, r}],
    {ponavljanja}];
  <|"rezultat" -> prvi, "redovi" -> redovi|>
]

SetAttributes[dugme, HoldRest]
dugme[tekst_, akcija_] := Button[If[StringQ[tekst], zag[tekst], tekst], akcija, Method -> "Queued", ImageSize -> Automatic]

SetAttributes[polje, HoldFirst]
(* Tip Expression umjesto Number: Number vraca priblizne (realne) brojeve (3. umjesto 3,
   1.*^6 umjesto 1000000), a kriptografske funkcije traze cijele brojeve. *)
cijeloIzUnosa[x_Real] /; x == Round[x] && Abs[x] < 2^53 := Round[x]
cijeloIzUnosa[x_] := x

polje[v_, opis_, sir_ : 10] :=
  Row[{Style[Row[{zag[opis], " = "}], Bold],
    InputField[Dynamic[v, Function[val, v = cijeloIzUnosa[val]]], Expression, FieldSize -> sir]}]

SetAttributes[poljeDugo, HoldFirst]
poljeDugo[v_, opis_] :=
  Row[{Style[Row[{zag[opis], " = "}], Bold], InputField[Dynamic[v, Function[val, v = cijeloIzUnosa[val]]], Expression, FieldSize -> {{16, 36}, {1, 4}}]}]

SetAttributes[poljeT, HoldFirst]
poljeT[v_, opis_, sir_ : 20] :=
  Row[{Style[Row[{zag[opis], " = "}], Bold], InputField[Dynamic[v], String, FieldSize -> sir]}]

panel[naslov_String, sadrzaj_] :=
  Panel[Column[{Style[zag[naslov], Bold, 15], sadrzaj}, Spacings -> 1, Alignment -> Left],
    ImageMargins -> 6]

blokovi[s_String] :=
  If[StringLength[s] > 48, StringRiffle[StringPartition[s, UpTo[48]], "\n"], s]

ocisti[s_String] := StringReplace[s, "Kriptografija`Private`O" -> "O"]

vr[v_Row] := v
vr[v_Integer] := Style[blokovi[ToString[v]], FontFamily -> "Courier"]
vr[v_String] := If[AllTrue[ToCharacterCode[v], # < 128 &],
  Style[ocisti[v], FontFamily -> "Courier"], Style[ocisti[v]]]
vr[v_List] /; Length[v] > 100 := Column[{
  Style[ocisti[ToString[Short[v, 8]]], FontFamily -> "Courier"],
  Style["(" <> ToString[Length[v]] <> " elemenata; prikaz je skra\[CAcute]en)", 11, Gray]}]
vr[v_] := Style[blokovi[ocisti[ToString[v, InputForm]]], FontFamily -> "Courier"]

(* izlaz funkcije kao u radu: poziv i vracena vrijednost *)
izv[ime_String, args_List, rez_] :=
  {ime <> "[" <> StringRiffle[ocisti[ToString[#, InputForm]] & /@ args, ", "] <> "]", rez}

tekstIzlaza[stavke_List] := StringRiffle[
  (#[[1]] <> " = " <> ocisti[ToString[#[[2]], InputForm]]) & /@ stavke, "\n"]

izvorno[stavke_List] := With[{t = tekstIzlaza[stavke]},
  Column[{
    Row[{Style["Izlaz funkcija (kao u radu)", Bold, 13], Spacer[15],
      Button["Kopiraj izlaz u tekstu", CopyToClipboard[t], ImageSize -> Automatic]}],
    Grid[Map[{Style[#[[1]], FontFamily -> "Courier", RGBColor[0.3, 0.3, 0.6]], Style["=", Bold],
        vr[#[[2]]]} &, stavke],
      Alignment -> {Left, Top}, Frame -> All, Spacings -> {1, 0.6}]
  }, Spacings -> 1]
]

(* Naslov drugog dijela odgovara onome sto se zaista prikazuje:
   koraci racunanja (tabele s natpisom pomocnog proracuna), slike, tabele vrijednosti *)
naslovDrugogDijela[vizual_] := Module[{k, g, t},
  k = !FreeQ[vizual, pomocniNatpis];
  g = !FreeQ[vizual, _Graphics | _Graphics3D | _Image];
  t = !FreeQ[vizual, _Grid];
  Which[
    MatchQ[vizual, Style[_, RGBColor[1, 0, 0], ___]], None,
    k && g, "Koraci ra\[CHacek]unanja i vizualizacija",
    k, "Koraci ra\[CHacek]unanja",
    g && t, "Vrijednosti i vizualizacija",
    g, "Vizualizacija",
    t, "Dodatne vrijednosti",
    True, None]
]

dvaDijela[stavke_List, vizual_] := dvaDijela[stavke, vizual, Automatic]

dvaDijela[stavke_List, vizual_, naslov_] := Module[{nas},
  nas = If[naslov === Automatic, naslovDrugogDijela[vizual], naslov];
  Column[{
    izvorno[stavke],
    If[nas === None, Nothing, Style[nas, Bold, 13]],
    vizual
  }, Spacings -> 1.2]
]

(* provjere ispravnosti: izlaz je True ili lista opisa problema *)
provjeraPrikaz[ime_String, args_List, r_, extra_: Nothing] :=
  If[r === True || (ListQ[r] && r =!= {} && AllTrue[r, StringQ]),
    dvaDijela[{izv[ime, args, r]}, Column[{ispisProvjere[r], extra}, Spacings -> 1], "Rezultat provjere"],
    r]

(* binarni prikaz: 1 = crno, 0 = bijelo *)
bitTraka[bitovi_List, naslov_String] := Module[{w, redovi},
  w = Min[Length[bitovi], 64];
  redovi = Partition[PadRight[bitovi, w*Ceiling[Length[bitovi]/w]], w];
  ArrayPlot[redovi, ColorRules -> {0 -> White, 1 -> Black}, Mesh -> (Length[bitovi] <= 1024),
    Frame -> True, FrameTicks -> None, ImageSize -> Min[620, 20 + 11*w],
    PlotLabel -> naslov <> " (binarno: 1 = crno, 0 = bijelo; " <> ToString[Length[bitovi]] <> " bitova)"]
]

(* bitovi kao kvadratna matrica (npr. 400 bitova kao 20x20); posljednji red se dopunjuje bijelim *)
bitMatrica[bitovi_List, naslov_String] := Module[{w, redovi},
  w = Ceiling[Sqrt[Length[bitovi]]];
  redovi = Partition[PadRight[bitovi, w*w], w];
  ArrayPlot[redovi, ColorRules -> {0 -> White, 1 -> Black}, Mesh -> (Length[bitovi] <= 1024),
    Frame -> True, FrameTicks -> None, ImageSize -> Min[420, 40 + 12*w],
    PlotLabel -> naslov <> " (matrica " <> ToString[w] <> "x" <> ToString[w] <> ")"]
]

binTraka[broj_Integer, naslov_String] := bitTraka[IntegerDigits[broj, 2], naslov]

(* realni brojevi u tabelama na tri znacajne cifre *)
tri[x_Real] := With[{s = ToString[NumberForm[x, 3]]}, If[StringEndsQ[s, "."], StringDrop[s, -1], s]]
tri[x_] := x

tablica[redovi_List] := Grid[
  Map[{Style[zag[#[[1]]], Bold], If[Head[#[[2]]] === Real, Style[tri[#[[2]]], FontFamily -> "Courier"], vr[#[[2]]]]} &, redovi],
  Alignment -> {Left, Center}, Frame -> All, Spacings -> {1.5, 0.8},
  Background -> {{GrayLevel[0.93], None}}
]

skrati[v_] := Module[{s = ToString[v]},
  If[StringLength[s] > 22,
    StringTake[s, 9] <> "..." <> StringTake[s, -6] <> " (" <> ToString[StringLength[s]] <> " cif.)",
    s
  ]
]

prikaziTacku[t_] := If[ListQ[t], t, "O (ta\[CHacek]ka u beskona\[CHacek]nosti)"]

razdvoji[s_String] := ToExpression /@ StringSplit[s, RegularExpression["[,;\\s]+"]]

(* shema: dvije strane i strelice izmedju njih *)
shema[ime1_String, linije1_List, ime2_String, linije2_List, strelice_List] := Column[{Graphics[{
  EdgeForm[GrayLevel[0.4]], FaceForm[RGBColor[0.85, 0.92, 1]],
  Rectangle[{0, 0}, {4, 6}, RoundingRadius -> 0.3],
  FaceForm[RGBColor[0.86, 0.96, 0.86]],
  Rectangle[{10, 0}, {14, 6}, RoundingRadius -> 0.3],
  Black,
  Text[Style[zag[ime1], Bold, 14], {2, 5.4}],
  Text[Style[zag[ime2], Bold, 14], {12, 5.4}],
  MapIndexed[Text[Style[zag[#1], 11], {2, 4.4 - 0.8 (#2[[1]] - 1)}] &, linije1],
  MapIndexed[Text[Style[zag[#1], 11], {12, 4.4 - 0.8 (#2[[1]] - 1)}] &, linije2],
  MapIndexed[
    With[{y = 4.2 - 1.2 (#2[[1]] - 1)},
      {Arrowheads[0.035], Darker[Red],
        If[#1[[1]] === "->", Arrow[{{4.2, y}, {9.8, y}}], Arrow[{{9.8, y}, {4.2, y}}]],
        Black, Text[Style[zag[#1[[2]]], 11], {7, y + 0.35}]}
    ] &, strelice]
  },
  ImageSize -> 700, PlotRange -> {{-0.3, 14.3}, {-0.3, 6.3}}
  ]}]

(* tacke eliptickih krivulja nad F_p *)
tackeKrivulje[a_, b_, p_] := Module[{kor},
  kor = GroupBy[Range[0, p - 1], Mod[#^2, p] &];
  Flatten[
    Table[{x, y}, {x, 0, p - 1}, {y, Lookup[kor, Mod[x^3 + a*x + b, p], {}]}],
    1]
]

grafKrivulje[a_, b_, p_, orbita_List : {}, istaknute_List : {}] := Module[{tacke, oz},
  tacke = tackeKrivulje[a, b, p];
  oz = Table[
    If[ListQ[i[[3]]],
      {i[[1]], PointSize[0.03], Point[i[[3]]], Text[Style[i[[2]], Bold, 12], i[[3]], {-1.3, -1.3}]},
      Nothing],
    {i, istaknute}];
  Show[
    ListPlot[tacke, PlotStyle -> {GrayLevel[0.4], PointSize[0.012]}, Frame -> True, Axes -> False,
      GridLines -> Automatic, PlotRange -> {{-1, p}, {-1, p}}, AspectRatio -> 1,
      ImageSize -> 440, FrameLabel -> {"x", "y"},
      PlotLabel -> zag["Ta\[CHacek]ke krivulje nad F_p"]],
    Graphics[{Orange, PointSize[0.016], Point[Select[orbita, ListQ]]}],
    Graphics[oz]
  ]
]

realnaKrivulja[a_, b_] := If[Abs[a] <= 12 && Abs[b] <= 12,
  ContourPlot[y^2 == x^3 + a*x + b, {x, -5, 6}, {y, -12, 12},
    ContourStyle -> Blue, ImageSize -> 330,
    PlotLabel -> "Ista krivulja nad R (ilustracija)"],
  Nothing
]


(* ------------------------------------------------------------------ *)
(* Uputa                                                              *)
(* ------------------------------------------------------------------ *)

uputa[] := Column[{
  Style["Kriptografija javnog klju\[CHacek]a: grafi\[CHacek]ki interfejs", Bold, 20],
  Style["Interfejs poziva funkcije iz paketa Kriptografija.wl; sva ra\[CHacek]unanja izvr\[SHacek]avaju te funkcije.", 13],
  Style["1. Odaberite karticu u gornjem redu.", 12],
  Style["2. Upi\[SHacek]ite ulazne podatke u polja (cijeli brojevi; izrazi poput 2^64 su dozvoljeni).", 12],
  Style["3. Pritisnite dugme. Prvo se prikazuje izlaz funkcije iz paketa onakav kakav je u radu (uz dugme za kopiranje), a ispod njega tabele koraka ra\[CHacek]unanja i vizualizacija. Tabele koraka ponavljaju algoritme iz paketa i slu\[ZHacek]e u edukativne svrhe; kona\[CHacek]an rezultat uvijek daje funkcija paketa.", 12],
  Style["4. Kod kriptosistema klju\[CHacek]eve mo\[ZHacek]ete generisati ili upisati ru\[CHacek]no (npr. mali brojevi iz primjera), a zatim \[SHacek]ifrirati i de\[SHacek]ifrirati poruku.", 12],
  Style["5. Kod faktorizacije je zadano vremensko ograni\[CHacek]enje po metodi; ako se prekora\[CHacek]i, metoda se prekida.", 12],
  Style["6. Vremensko ograni\[CHacek]enje u zaglavlju prozora va\[ZHacek]i za svaki izra\[CHacek]un (0 = bez ograni\[CHacek]enja).", 12]
}, Spacings -> 1.2]


(* ------------------------------------------------------------------ *)
(* Kartica: Brojevi (teorija brojeva)                                  *)
(* ------------------------------------------------------------------ *)

rEuklid[a_, b_] := Which[
  !cijeliQ[{a, b}], neCijeli,
  b <= 0, greska["Potrebno je b > 0."],
  True, Module[{eu, pe, nd, ns, inv, stavke},
    eu = EuklidovAlgoritam[a, b];
    pe = ProsireniEuklid[a, b];
    nd = nzd[a, b];
    ns = nzs[a, b];
    inv = If[b > 1 && nd == 1, MultiplikativniInverz[a, b], None];
    stavke = {izv["EuklidovAlgoritam", {a, b}, eu], izv["ProsireniEuklid", {a, b}, pe],
      izv["nzd", {a, b}, nd], izv["nzs", {a, b}, ns]};
    If[inv =!= None, AppendTo[stavke, izv["MultiplikativniInverz", {a, b}, inv]]];
    dvaDijela[stavke, Column[{
      tablica[{
        "nzd(a,b)" -> nd,
        "Koli\[CHacek]nici q_i" -> eu[[1]],
        "Provjera a\[CenterDot]x + b\[CenterDot]y" -> a*pe[[2]] + b*pe[[3]],
        "Inverz od a modulo b" -> If[inv === None, "ne postoji", inv]}],
      koraciEuklid[a, b],
      koraciProsireni[a, b],
      If[IntegerQ[inv], koraciInverz[a, b, inv], Nothing]
    }, Spacings -> 1]]
  ]
]

rVerizni[a_, b_] := Which[
  !cijeliQ[{a, b}], neCijeli,
  b <= 0, greska["Potrebno je b > 0."],
  True, Module[{q, konv, vrijednosti},
    q = ParcijalniKvocijenti[a, b];
    konv = Konvergente[q];
    vrijednosti = N[#[[1]]/#[[2]], 15] & /@ konv;
    dvaDijela[{izv["ParcijalniKvocijenti", {a, b}, q], izv["Konvergente", {q}, konv]},
      Column[{
        koraciKonv[q],
        ListPlot[vrijednosti, DataRange -> {0, Length[vrijednosti] - 1}, Joined -> True, PlotMarkers -> Automatic, Frame -> True,
          PlotRange -> All, ImageSize -> 400, FrameLabel -> {"i", zag["p_i/q_i"]},
          FrameTicks -> {{Automatic, None}, {Range[0, Length[vrijednosti] - 1], None}},
          PlotLabel -> "Konvergente i vrijednost a/b (crvena isprekidana)",
          Epilog -> {Red, Dashed, Line[{{0, N[a/b]}, {Max[1, Length[vrijednosti] - 1], N[a/b]}}]}]
      }, Spacings -> 1]]
  ]
]

rKTO[sa_String, sm_String] := Module[{a, m, x, M},
  a = razdvoji[sa];
  m = razdvoji[sm];
  Which[
    a === {} || m === {} || !cijeliQ[a] || !cijeliQ[m], greska["Unesite cijele brojeve odvojene zarezom."],
    Length[a] != Length[m], greska["Liste moraju imati istu du\[ZHacek]inu."],
    AnyTrue[m, # < 2 &], greska["Moduli moraju biti \[GreaterEqual] 2."],
    !AllTrue[Subsets[m, {2}], nzd[#[[1]], #[[2]]] == 1 &], greska["Moduli moraju biti u parovima relativno prosti."],
    True, (
      x = KineskiTeoremOOstacima[a, m];
      M = Times @@ m;
      dvaDijela[{izv["KineskiTeoremOOstacima", {a, m}, x]}, Column[{
        tablica[{"M = m_1\[CenterDot]...\[CenterDot]m_s" -> M,
          "x mod m_j" -> Mod[x, m], "Zadani ostaci a_j mod m_j" -> Mod[a, m],
          "Rje\[SHacek]enje ispravno" -> (Mod[x, m] === Mod[a, m])}],
        koraciKTO[a, m],
        If[M <= 200, ktoPrava[a, m, x], objasni["(prikaz na brojevnoj pravi samo za M \[LessEqual] 200)"]]
      }, Spacings -> 1]]
    )
  ]
]

rModPot[a_, e_, m_] := Which[
  !cijeliQ[{a, e, m}], neCijeli,
  m < 2 || e < 0, greska["Potrebno je m \[GreaterEqual] 2 i eksponent \[GreaterEqual] 0."],
  True, Module[{rez, red, phi, fakt, jac, stavke},
    rez = ModPotenciranje[a, e, m];
    stavke = {izv["ModPotenciranje", {a, e, m}, rez]};
    red = None;
    If[nzd[a, m] == 1 && m <= 10^6,
      phi = EulerovaFunkcija[m];
      fakt = PotpunaFaktorizacija[phi];
      red = RedElementa[a, m, fakt];
      AppendTo[stavke, izv["EulerovaFunkcija", {m}, phi]];
      AppendTo[stavke, izv["PotpunaFaktorizacija", {phi}, fakt]];
      AppendTo[stavke, izv["RedElementa", {a, m, fakt}, red]]
    ];
    If[OddQ[m] && m >= 3,
      jac = JacobijevSimbol[a, m];
      AppendTo[stavke, izv["JacobijevSimbol", {a, m}, jac]]
    ];
    dvaDijela[stavke, Column[{
      tablica[{
        "Red elementa a modulo m" -> If[IntegerQ[red], red, "(a nije relativno prost s m, ili je m > 10^6)"],
        "a je primitivni korijen" -> If[IntegerQ[red], red == phi, "-"]}],
      koraciModPot[a, e, m]
    }, Spacings -> 1]]
  ]
]

imaPrimitivniKorijen[n_] := Module[{fz = Sort[PotpunaFaktorizacija[n]]},
  n == 2 || n == 4 ||
  (Length[fz] == 1 && fz[[1, 1]] > 2) ||
  (Length[fz] == 2 && fz[[1]] == {2, 1} && fz[[2, 1]] > 2)
]

rPrimKorijen[n_] := Which[
  !cijeliQ[{n}], neCijeli,
  n < 3, greska["Potrebno je n \[GreaterEqual] 3."],
  !imaPrimitivniKorijen[n], greska["Primitivni korijen modulo n postoji samo za n = 2, 4, p^k i 2p^k (p neparan prost)."],
  True, Module[{phi, fakt, g1, g2, L},
    phi = EulerovaFunkcija[n];
    fakt = PotpunaFaktorizacija[phi];
    g1 = PrimitivniKorijen[n, fakt];
    g2 = PrimitivniKorijenNajmanji[n, fakt];
    L = Min[phi, 400];
    dvaDijela[{izv["EulerovaFunkcija", {n}, phi], izv["PotpunaFaktorizacija", {phi}, fakt],
        izv["PrimitivniKorijen", {n, fakt}, g1], izv["PrimitivniKorijenNajmanji", {n, fakt}, g2]},
      Column[{
        tablica[{"Broj primitivnih korijena \[Phi](\[Phi](n))" -> EulerovaFunkcija[phi]}]
      }, Spacings -> 1]]
  ]
]

pEuklid[] := DynamicModule[{a = 1071, b = 462, rez = ""},
  panel["Euklidov algoritam, pro\[SHacek]ireni Euklid, inverz", Column[{
    Row[{polje[a, "a"], Spacer[15], polje[b, "b"], Spacer[15],
      dugme["Izra\[CHacek]unaj", rez = izlaz[rEuklid[a, b]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pVerizni[] := DynamicModule[{a = 415, b = 93, rez = ""},
  panel["Veri\[ZHacek]ni razlomak i konvergente", Column[{
    Row[{polje[a, "a"], Spacer[15], polje[b, "b"], Spacer[15],
      dugme["Izra\[CHacek]unaj", rez = izlaz[rVerizni[a, b]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pKTO[] := DynamicModule[{sa = "2, 3, 2", sm = "3, 5, 7", rez = ""},
  panel["Kineski teorem o ostacima", Column[{
    Row[{poljeT[sa, "ostaci a_j", 18], Spacer[15], poljeT[sm, "moduli m_j", 18], Spacer[15],
      dugme["Rije\[SHacek]i", rez = izlaz[rKTO[sa, sm]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pModPot[] := DynamicModule[{a = 7, e = 128, m = 13, rez = ""},
  panel["Modularno potenciranje, red elementa, Jacobijev simbol", Column[{
    Row[{polje[a, "a"], Spacer[15], polje[e, "e"], Spacer[15], polje[m, "m"], Spacer[15],
      dugme["Izra\[CHacek]unaj", rez = izlaz[rModPot[a, e, m]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pPrim[] := DynamicModule[{n = 23, rez = ""},
  panel["Primitivni korijen modulo n", Column[{
    Row[{polje[n, "n"], Spacer[15], dugme["Prona\:0111i primitivne korijene", rez = izlaz[rPrimKorijen[n]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

tabBrojevi[] := Column[{pEuklid[], pVerizni[], pKTO[], pModPot[], pPrim[]}, Spacings -> 1]


(* ------------------------------------------------------------------ *)
(* Kartica: Prosti brojevi                                             *)
(* ------------------------------------------------------------------ *)

ishod[True] := Style["vjerovatno prost", Darker[Green], Bold]
ishod[False] := Style["sigurno slo\[ZHacek]en", Red, Bold]
ishod[x_] := x

grafGreske[k_] := Module[{K = Max[k, 10], f},
  f = Table[N[2.^(-j)], {j, 1, K}];
  ListLogPlot[{f, f, Table[N[4.^(-j)], {j, 1, K}]},
    Joined -> True, Frame -> True, ImageSize -> 440,
    PlotStyle -> {Directive[RGBColor[0.9, 0.5, 0.1], Thickness[0.012], Opacity[0.45]],
      Directive[RGBColor[0.25, 0.4, 0.75], Dashing[{0.02, 0.02}]], RGBColor[0.2, 0.6, 0.3]},
    PlotMarkers -> {{Graphics[{RGBColor[0.9, 0.5, 0.1], Disk[]}], 12}, {Graphics[{RGBColor[0.25, 0.4, 0.75], Rectangle[]}], 7},
      {Graphics[{RGBColor[0.2, 0.6, 0.3], Polygon[{{0, 1}, {-1, -1}, {1, -1}}]}], 9}},
    FrameLabel -> {"k", "gornja granica gre\[SHacek]ke"},
    PlotLegends -> {zag["Fermat (za brojeve koji nisu Carmichaelovi): 2^(-k)"],
      zag["Solovay\[Dash]Strassen: 2^(-k)"], zag["Miller\[Dash]Rabin: 4^(-k)"]},
    PlotLabel -> "Gornja granica vjerovatno\[CAcute]e gre\[SHacek]ke"]
]

rTestovi[n_, k_, prob_] := Which[
  !cijeliQ[{n, k}], neCijeli,
  n < 3 || k < 1, greska["Potrebno je n \[GreaterEqual] 3 i k \[GreaterEqual] 1."],
  !(NumberQ[prob] && 0 < prob < 1), greska["Vjerovatno\[CAcute]a p mora biti u intervalu (0,1)."],
  True, Module[{f, ss, mr, tf, tss, tmr, kf, km},
    {tf, f} = AbsoluteTiming[FermatovTest[n, k]];
    {tss, ss} = AbsoluteTiming[SolovayStrassenTest[n, k]];
    {tmr, mr} = AbsoluteTiming[MillerRabinTest[n, k]];
    kf = PotrebnoIteracijaFSS[prob];
    km = PotrebnoIteracijaMillerRabin[prob];
    dvaDijela[{izv["FermatovTest", {n, k}, f], izv["SolovayStrassenTest", {n, k}, ss],
        izv["MillerRabinTest", {n, k}, mr], izv["PotrebnoIteracijaFSS", {prob}, kf],
        izv["PotrebnoIteracijaMillerRabin", {prob}, km]},
      Column[{
        Grid[{
          {Style["Test", Bold], Style["Ishod", Bold], Style["t [s]", Bold]},
          {"Fermat", ishod[f], tri[tf]},
          {"Solovay\[Dash]Strassen", ishod[ss], tri[tss]},
          {"Miller\[Dash]Rabin", ishod[mr], tri[tmr]},
          {"PrimeQ (referenca)", If[PrimeQ[n], Style["prost", Darker[Green], Bold], Style["slo\[ZHacek]en", Red, Bold]], ""}},
          Frame -> All, Spacings -> {1.5, 0.8}],
        objasni["Fermatov, Solovay\[Dash]Strassenov i Miller\[Dash]Rabinov test su vjerovatnosni: odgovor \"vjerovatno prost\" nije dokaz prostosti, a odgovor \"sigurno slo\[ZHacek]en\" jeste dokaz slo\[ZHacek]enosti. Rezultat zavisi od slu\[CHacek]ajno izabranih baza."],
        grafGreske[k],
        koraciTestovi[n, k]
      }, Spacings -> 1]]
  ]
]

rSito[gr_] := Which[
  !cijeliQ[{gr}], neCijeli,
  gr < 2, greska["Potrebno je n \[GreaterEqual] 2."],
  True, Module[{pr},
    pr = EratostenovoSito[gr];
    dvaDijela[{izv["EratostenovoSito", {gr}, pr]}, Column[{
      tablica[{"Broj prostih \[LessEqual] n" -> Length[pr],
        "Najve\[CAcute]i prost \[LessEqual] n" -> Last[pr]}],
      objasni["Eratostenovo sito je determinisano: svaki prikazani broj je dokazano prost (za razliku od vjerovatnosnih testova)."],
      Show[
        ListStepPlot[Transpose[{pr, Range[Length[pr]]}], PlotStyle -> Blue, Frame -> True,
          ImageSize -> 440, FrameLabel -> {"x", "\[Pi](x)"}],
        Plot[x/Log[x], {x, 2, gr}, PlotStyle -> {Red, Dashed}],
        PlotLabel -> "\[Pi](x) (plavo) i x/ln x (crveno, isprekidano)"]
    }, Spacings -> 1]]
  ]
]

rBBS[m_, l_] := Which[
  !cijeliQ[{m, l}], neCijeli,
  m < 5 || l < 1, greska["Potrebno je m \[GreaterEqual] 5 i l \[GreaterEqual] 1."],
  True, Module[{init, gen, bitovi, broj, uzorak, brojevi},
    pokreniBBS[m];
    init = BBSInicijalizacija[m];
    gen = BBSGenerisiBitove[init[[1]], init[[2]], Min[l, 16]];
    bitovi = BBSSljedeciBitovi[l];
    broj = BBSBroj[l];
    uzorak = BBSBrojIzOpsega[1, 10];
    brojevi = Table[BBSBrojIzOpsega[1, 10], {2000}];
    dvaDijela[{izv["BBSInicijalizacija", {m}, init],
        izv["BBSGenerisiBitove", {init[[1]], init[[2]], Min[l, 16]}, gen],
        izv["BBSSljedeciBitovi", {l}, bitovi], izv["BBSBroj", {l}, broj],
        izv["BBSBrojIzOpsega", {1, 10}, uzorak]},
      Column[{
        binTraka[broj, "BBSBroj[" <> ToString[l] <> "]"],
        DynamicModule[{prikaz = "traka"},
          Column[{
            Row[{Style["Prikaz niza bitova: ", Bold], RadioButtonBar[Dynamic[prikaz], {"traka" -> "traka", "matrica" -> "matrica"}]}],
            Dynamic[If[prikaz === "traka",
              bitTraka[bitovi, "BBSSljedeciBitovi[" <> ToString[l] <> "]"],
              bitMatrica[bitovi, "BBSSljedeciBitovi[" <> ToString[l] <> "]"]]]}, Spacings -> 0.6]],
        tablica[{"Broj nula u nizu bitova" -> l - Total[bitovi], "Broj jedinica u nizu bitova" -> Total[bitovi],
          "Udio jedinica" -> N[Total[bitovi]/l]}],
        slucajnaNapomena,
        Histogram[brojevi, {0.5, 10.5, 1}, ImageSize -> 440,
          PlotLabel -> "2000 brojeva iz [1,10] (BBSBrojIzOpsega)"]
      }, Spacings -> 1]]
  ]
]

rProsti[m_, k_, n_] := Which[
  !cijeliQ[{m, k, n}], neCijeli,
  m < 5 || k < 1 || n < 1, greska["Potrebno je m \[GreaterEqual] 5, k \[GreaterEqual] 1, n \[GreaterEqual] 1."],
  True, Module[{t, pr, sljedeci, stat},
    osiguraBBS[m];
    {t, pr} = AbsoluteTiming[SlucajanProst[m, k]];
    sljedeci = SljedeciProst[n, k];
    stat = Table[SlucajanProstBrojPokusaja[m, k, 10000], {30}];
    dvaDijela[{izv["SlucajanProst", {m, k}, pr], izv["SljedeciProst", {n, k}, sljedeci],
        izv["SlucajanProstBrojPokusaja", {m, k, 10000}, stat[[1]]]},
      Column[{
        slucajnaNapomena,
        binTraka[pr, "SlucajanProst[" <> ToString[m] <> "]"],
        tablica[{"Bitovna du\[ZHacek]ina" -> IntegerLength[pr, 2], "Vrijeme [s]" -> t,
          "Potvrda (PrimeQ)" -> PrimeQ[pr], "Potvrda sljede\[CAcute]eg (PrimeQ)" -> PrimeQ[sljedeci],
          "Prosje\[CHacek]an broj generisanih kandidata (30 ponavljanja)" -> N[Mean[stat[[All, 2]]]],
          "Prosje\[CHacek]an broj kandidata poslanih na Miller\[Dash]Rabin" -> N[Mean[stat[[All, 3]]]]}]
      }, Spacings -> 1]]
  ]
]

pTestovi[] := DynamicModule[{n = 104729, k = 10, prob = 0.999999, rez = ""},
  panel["Testovi prostosti", Column[{
    Row[{polje[n, "n", 14], Spacer[15], polje[k, "k"], Spacer[15], polje[prob, "p", 10], Spacer[15],
      dugme["Testiraj", rez = izlaz[rTestovi[n, k, prob]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pSito[] := DynamicModule[{gr = 1000, rez = ""},
  panel["Eratostenovo sito", Column[{
    Row[{polje[gr, "n", 10], Spacer[15], dugme["Prona\:0111i proste brojeve", rez = izlaz[rSito[gr]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pBBS[] := DynamicModule[{m = 32, l = 512, rez = ""},
  panel["Blum\[Dash]Blum\[Dash]Shub generator", Column[{
    Row[{polje[m, "bitovna du\[ZHacek]ina prostih m"], Spacer[15], polje[l, "broj bitova l"], Spacer[15],
      dugme["Pokreni i generi\[SHacek]i", rez = izlaz[rBBS[m, l]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

pProsti[] := DynamicModule[{m = 64, k = 20, n = 1000000, rez = ""},
  panel["Slu\[CHacek]ajan prost broj i sljede\[CAcute]i prost broj", Column[{
    Row[{polje[m, "m (bitova)"], Spacer[15], polje[k, "k"], Spacer[15], polje[n, "n", 12], Spacer[15],
      dugme["Generi\[SHacek]i", rez = izlaz[rProsti[m, k, n]]]}],
    Dynamic[rez]}, Spacings -> 1]]
]

tabProsti[] := Column[{pTestovi[], pSito[], pBBS[], pProsti[]}, Spacings -> 1]


(* ------------------------------------------------------------------ *)
(* Kartica: Faktorizacija                                              *)
(* ------------------------------------------------------------------ *)

imenaMetoda = {"Probno dijeljenje", "Fermat", "Pollard p-1", "Pollard p-1 (pro\[SHacek]ireno)",
  "Pollard rho", "Pollard rho (Floyd)", "Pollard rho (Brent)", "Faktorska baza", "Dixon",
  "Kanonska faktorizacija"};

pokreniMetodu[ime_String, n_Integer, par_Association] := Switch[ime,
  "Probno dijeljenje", FaktorizacijaDijeljenjem[n],
  "Fermat", FermatovaFaktorizacija[n],
  "Pollard p-1", PollardovaPMinus1[n, par["Bmin"], par["Bmax"], par["a"]],
  "Pollard p-1 (pro\[SHacek]ireno)", PollardovaPMinus1Prosireno[n, par["Bmin"], par["Bmax"], 2, 5],
  "Pollard rho", PollardovaRho[n, par["x0"], par["c"]],
  "Pollard rho (Floyd)", PollardovaRhoFloyd[n, par["x0"], par["c"]],
  "Pollard rho (Brent)", PollardovaRhoBrent[n, par["x0"], par["c"]],
  "Faktorska baza", MetodaFaktorskeBaze[n, par["w"], par["maks"]],
  "Dixon", DixonovAlgoritam[n, par["w"], par["maks"]],
  "Kanonska faktorizacija", PotpunaFaktorizacija[n],
  _, "Nepoznata metoda."
]

stepen[{p_, e_}] := If[e == 1, ToString[p], ToString[p] <> "^" <> ToString[e]]

oblikRezultata[r_Integer, n_] := {1 < r < n && Divisible[n, r],
  ToString[r] <> " \[CenterDot] " <> ToString[Quotient[n, r]]}
oblikRezultata[r_String, n_] := {False, r}
oblikRezultata[{p_Integer, q_Integer}, n_] := {p*q == n, ToString[p] <> " \[CenterDot] " <> ToString[q]}
oblikRezultata[r_List, n_] := If[r =!= {} && AllTrue[r, (ListQ[#] && Length[#] == 2) &],
  {Times @@ (#[[1]]^#[[2]] & /@ r) == n, StringRiffle[stepen /@ r, " \[CenterDot] "]},
  {False, ToString[r]}]
oblikRezultata[r_, n_] := {False, ToString[r]}

rFakt[n_, odabrane_, par_, ogr_, rep_] := Which[
  !cijeliQ[{n, ogr, rep}], neCijeli,
  n < 4 || ogr < 0 || rep < 1, greska["Potrebno je n \[GreaterEqual] 4, vremensko ograni\[CHacek]enje \[GreaterEqual] 0 s (0 = bez ograni\[CHacek]enja) i broj ponavljanja \[GreaterEqual] 1."],
  odabrane === {}, greska["Odaberite barem jednu metodu."],
  True, Module[{rez, redovi, imena, vremena, stavke, vise = rep > 1, zagl},
    (* mjeri se samo poziv funkcije paketa (izmjeri); tabele i grafikoni se prave poslije mjerenja *)
    rez = Table[
      Module[{m = izmjeri[ime, n, par, ogr, rep], r, uspjesni, nepr, tvr, tmed, svi},
        r = m["rezultat"];
        svi = m["redovi"];
        uspjesni = Select[svi, oblikRezultata[#[[2]], n][[1]] &];
        nepr = Select[svi, #[[2]] =!= $prekoraceno &];
        tvr = If[uspjesni =!= {}, uspjesni[[All, 1]], nepr[[All, 1]]];
        tmed = If[tvr === {}, If[ogr > 0, N[ogr], 0.], Median[tvr]];
        <|"ime" -> ime, "r" -> r, "tmed" -> tmed,
          "tmin" -> If[tvr === {}, None, Min[tvr]], "tmax" -> If[tvr === {}, None, Max[tvr]],
          "ok" -> Length[uspjesni], "prek" -> Length[svi] - Length[nepr],
          "neusp" -> Length[nepr] - Length[uspjesni], "sviPrekoraceno" -> (tvr === {})|>],
      {ime, odabrane}];
    stavke = Map[{pozivMetode[#["ime"], n, par], #["r"]} &, rez];
    zagl = Join[{Style["Metoda", Bold], Style["Rezultat", Bold], Style["Status", Bold],
      Style[If[vise, "t medijana [s]", "t [s]"], Bold]},
      If[vise, {Style["t min [s]", Bold], Style["t max [s]", Bold], Style["uspjeha / neuspjeha / prekora\[CHacek]enja", Bold]}, {}]];
    redovi = Map[
      Function[z, Module[{o = oblikRezultata[z["r"], n], st},
        st = Which[
          o[[1]], "uspjeh",
          z["r"] === $prekoraceno, "vremensko ograni\[CHacek]enje",
          z["r"] === "Gre\[SHacek]ka pri izvr\[SHacek]avanju.", "gre\[SHacek]ka pri izvr\[SHacek]avanju",
          True, "neuspjeh metode"];
        Join[{z["ime"], Style[o[[2]], If[o[[1]], Darker[Green], Red]],
          Style[st, If[o[[1]], Darker[Green], Red], Bold],
          If[z["sviPrekoraceno"], ">" <> ToString[ogr], tri[N[z["tmed"]]]]},
          If[vise, {If[z["tmin"] === None, "-", tri[N[z["tmin"]]]], If[z["tmax"] === None, "-", tri[N[z["tmax"]]]],
            ToString[z["ok"]] <> " / " <> ToString[z["neusp"]] <> " / " <> ToString[z["prek"]]}, {}]]]],
      rez];
    imena = #["ime"] & /@ rez;
    vremena = Max[#["tmed"], 10^-5] & /@ rez;
    dvaDijela[stavke, Column[{
      Grid[Prepend[redovi, zagl], Frame -> All, Alignment -> Left, Spacings -> {1.5, 0.8}],
      objasni["Vrijeme je izmjereno samo za poziv funkcije iz paketa Kriptografija.wl (bez izrade tabela i grafikona)." <>
        If[vise, " Prikazani su medijana, najmanje i najve\[CAcute]e vrijeme od " <> ToString[rep] <> " ponavljanja; medijana se ra\[CHacek]una nad uspje\[SHacek]nim pokretanjima. ", " Jedno mjerenje (broj ponavljanja = 1) nije statisti\[CHacek]ki pouzdano. "] <>
        "Vremena su demonstrativna: zavise od ra\[CHacek]unara i optere\[CAcute]enja."],
      Sequence @@ Map[koraciFakt[#["ime"], n, par, #["r"], ogr] &, rez],
      With[{ok = Select[Transpose[{imena, vremena, rez}], oblikRezultata[#[[3]]["r"], n][[1]] &],
          bez = Select[Transpose[{imena, rez}], !oblikRezultata[#[[2]]["r"], n][[1]] &]},
        Column[{
          If[bez === {}, Nothing,
            objasni["Metode bez uspjeha nisu prikazane na grafikonu: " <> StringRiffle[#[[1]] & /@ bez, ", "] <> "."]],
          If[ok === {}, Nothing,
            BarChart[ok[[All, 2]], ChartLabels -> ok[[All, 1]], BarOrigin -> Left, ScalingFunctions -> "Log",
              LabelingFunction -> (Placed[Style[tri[N[#]] <> " s", 12, Black], After] &),
              BarSpacing -> 0.5, ImageSize -> 560, ImagePadding -> {{130, 80}, {30, 25}},
              PlotLabel -> If[vise, "Medijana vremena izvr\[SHacek]avanja [s] (logaritamska skala)", "Vrijeme izvr\[SHacek]avanja [s] (logaritamska skala)"]]]
        }, Spacings -> 1]]
    }, Spacings -> 1]]
  ]
]

pozivMetode[ime_String, n_Integer, par_Association] := Switch[ime,
  "Probno dijeljenje", "FaktorizacijaDijeljenjem[" <> ToString[n] <> "]",
  "Fermat", "FermatovaFaktorizacija[" <> ToString[n] <> "]",
  "Pollard p-1", "PollardovaPMinus1[" <> StringRiffle[ToString /@ {n, par["Bmin"], par["Bmax"], par["a"]}, ", "] <> "]",
  "Pollard p-1 (pro\[SHacek]ireno)", "PollardovaPMinus1Prosireno[" <> StringRiffle[ToString /@ {n, par["Bmin"], par["Bmax"], 2, 5}, ", "] <> "]",
  "Pollard rho", "PollardovaRho[" <> StringRiffle[ToString /@ {n, par["x0"], par["c"]}, ", "] <> "]",
  "Pollard rho (Floyd)", "PollardovaRhoFloyd[" <> StringRiffle[ToString /@ {n, par["x0"], par["c"]}, ", "] <> "]",
  "Pollard rho (Brent)", "PollardovaRhoBrent[" <> StringRiffle[ToString /@ {n, par["x0"], par["c"]}, ", "] <> "]",
  "Faktorska baza", "MetodaFaktorskeBaze[" <> StringRiffle[ToString /@ {n, par["w"], par["maks"]}, ", "] <> "]",
  "Dixon", "DixonovAlgoritam[" <> StringRiffle[ToString /@ {n, par["w"], par["maks"]}, ", "] <> "]",
  "Kanonska faktorizacija", "PotpunaFaktorizacija[" <> ToString[n] <> "]",
  _, ime
]

rPotpuna[n_] := Which[
  !cijeliQ[{n}], neCijeli,
  n < 2, greska["Potrebno je n \[GreaterEqual] 2."],
  True, Module[{fz, phi, t, rast},
    {t, fz} = AbsoluteTiming[PotpunaFaktorizacija[n]];
    rast = RastaviNaProsteFaktore[n];
    phi = EulerovaFunkcija[n];
    dvaDijela[{izv["RastaviNaProsteFaktore", {n}, rast], izv["PotpunaFaktorizacija", {n}, fz], izv["EulerovaFunkcija", {n}, phi]},
      Column[{
        tablica[{"n = ..." -> StringRiffle[stepen /@ fz, " \[CenterDot] "], "Vrijeme [s]" -> t}]
      }, Spacings -> 1]]
  ]
]

rMjerenje[odabrane_, bMin_, bMax_, korak_, ponavljanja_, par_, ogr_] := Which[
  !cijeliQ[{bMin, bMax, korak, ponavljanja, ogr}], neCijeli,
  bMin < 5 || bMax < bMin || korak < 1 || ponavljanja < 1 || ogr < 0,
    greska["Potrebno je 5 \[LessEqual] min \[LessEqual] max (bitova svakog prostog faktora), korak \[GreaterEqual] 1, ponavljanja \[GreaterEqual] 1."],
  odabrane === {}, greska["Odaberite barem jednu metodu."],
  True, Module[{duzine, brojevi, podaci, tacke, tabBrojanje},
    duzine = Range[bMin, bMax, korak];
    (* za svaku bitovnu duzinu unaprijed se generise isti skup brojeva n = p q,
       a sve odabrane metode se izvrsavaju nad tim istim brojevima *)
    brojevi = Table[
      pokreniBBS[bits];
      Table[
        Module[{p, q},
          p = SlucajanProst[bits];
          q = SlucajanProst[bits];
          While[q == p, q = SlucajanProst[bits]];
          p*q],
        {ponavljanja}],
      {bits, duzine}];
    (* podaci[[metoda, duzina]] = {bits, medijana uspjesnih, uspjeha, neuspjeha, prekoracenja}.
       Mjeri se samo poziv funkcije paketa (izmjeri), a status se odredjuje poslije mjerenja. *)
    podaci = Table[
      Table[
        Module[{rez, tv, st},
          rez = Table[First[izmjeri[ime, n, par, ogr, 1]["redovi"]] /. {t_, r_} :> {t, r, n}, {n, brojevi[[k]]}];
          st = Map[Function[z, Which[
              z[[2]] === $prekoraceno, "prek",
              oblikRezultata[z[[2]], z[[3]]][[1]], "ok",
              True, "neusp"]], rez];
          tv = Pick[rez[[All, 1]], st, "ok"];
          {duzine[[k]], If[tv === {}, None, Max[10^-5, Median[tv]]],
            Count[st, "ok"], Count[st, "neusp"], Count[st, "prek"]}],
        {k, Length[duzine]}],
      {ime, odabrane}];
    (* tacke se crtaju samo tamo gdje je bar jedno izvrsavanje bilo uspjesno *)
    tacke = Map[Cases[#, {x_, m_?NumericQ, __} :> {x, m}] &, podaci];
    tabBrojanje = Column[{
      Style["Ishod izvr\[SHacek]avanja: uspjeh / neuspjeh / prekora\[CHacek]enje (od " <> ToString[ponavljanja] <> " ponavljanja)", Bold, 12],
      Grid[Prepend[
        MapThread[Prepend[#2, #1] &, {odabrane,
          Map[Function[z, Style[ToString[z[[3]]] <> " / " <> ToString[z[[4]]] <> " / " <> ToString[z[[5]]],
              If[z[[4]] + z[[5]] > 0, RGBColor[0.8, 0, 0], Black]]], podaci, {2}]}],
        Prepend[Style["m = " <> ToString[#], Bold] & /@ duzine, Style["metoda", Bold]]],
        Frame -> All, Alignment -> Center, Spacings -> {1, 0.6}],
      objasni["Medijana se ra\[CHacek]una samo od uspje\[SHacek]nih izvr\[SHacek]avanja (prona\:0111ena je ispravna faktorizacija). Sve metode se izvr\[SHacek]avaju nad istim brojevima. Ako uspjeha nema, ta\:010dka nije prikazana."]
    }, Spacings -> 0.8];
    Column[{
      ListLogPlot[tacke, Joined -> True, PlotMarkers -> Automatic, Frame -> True,
        ImageSize -> 520, PlotLegends -> odabrane,
        FrameLabel -> {"bitovna du\[ZHacek]ina m prostih faktora p i q", "medijana vremena [s]"},
        PlotLabel -> "Medijana vremena faktorizacije n = p\[CenterDot]q u zavisnosti od bitovne du\[ZHacek]ine m faktora"],
      tabBrojanje}, Spacings -> 1]
  ]
]

tabFakt[] := DynamicModule[{n = 101000661900893, odabrane = {"Fermat", "Pollard rho (Floyd)", "Pollard rho (Brent)"},
  Bmin = 10, Bmax = 2000, aP = 2, x0 = 2, cR = 1, w = 200, maks = 20000, ogr = 20, rep1 = 1,
  rez = "", rez2 = "", n2 = 101000661900893, rez3 = "",
  bMin = 10, bMax = 24, korak = 2, ponav = 3, odabrane3 = {"Pollard rho (Brent)", "Probno dijeljenje"},
  primjer = "A", podaci, rezP = ""},
  Column[{
    panel["Pore\:0111enje metoda", Column[{
      Row[{poljeDugo[n, "n"]}],
      Row[{Column[{Style["Metode", Bold],
          CheckboxBar[Dynamic[odabrane], imenaMetoda, Appearance -> "Vertical"]}],
        Spacer[25],
        Column[{Style["Parametri", Bold],
          polje[Bmin, "B_min", 6], polje[Bmax, "B_max", 6], polje[aP, "a (p-1)", 6],
          polje[x0, "x_0 (rho)", 6], polje[cR, "c (rho)", 6],
          polje[w, "w (baza)", 6], polje[maks, "poku\[SHacek]aji", 6],
          polje[ogr, "vrem. ograni\[CHacek]enje po metodi [s], 0 = bez", 6],
          polje[rep1, "broj ponavljanja mjerenja", 6]}, Spacings -> 0.5]}],
      dugme["Pokreni odabrane metode",
        rez = izlazBez[rFakt[n, odabrane,
          <|"Bmin" -> Bmin, "Bmax" -> Bmax, "a" -> aP, "x0" -> x0, "c" -> cR, "w" -> w, "maks" -> maks|>, ogr, rep1],
          10 + Length[odabrane]*(ogr + 5)*Max[rep1, 1]]],
      Dynamic[rez]}, Spacings -> 1]],
    panel["Primjeri strukture broja (iste veli\[CHacek]ine, razli\[CHacek]ita struktura faktora)", Column[{
      Row[{Style["Primjer: ", Bold], RadioButtonBar[Dynamic[primjer], {"A", "B", "C"}]}],
      Dynamic[Style[primjeriStrukture[primjer]["opis"], 12]],
      objasni["Brojevi su odabrani i provjereni funkcijama paketa. Sama bitovna du\[ZHacek]ina nije dovoljna za procjenu te\[ZHacek]ine faktorizacije: broj, metode i vremensko ograni\[CHacek]enje upisuju se u gornji panel i metode se pokre\[CAcute]u s trenutnim brojem ponavljanja."],
      dugme["U\[CHacek]itaj primjer i uporedi metode",
        (podaci = primjeriStrukture[primjer];
         n = podaci["n"]; odabrane = podaci["metode"]; Bmin = podaci["Bmin"]; Bmax = podaci["Bmax"]; ogr = podaci["ogr"];
         rezP = izlazBez[rFakt[n, odabrane,
           <|"Bmin" -> Bmin, "Bmax" -> Bmax, "a" -> aP, "x0" -> x0, "c" -> cR, "w" -> w, "maks" -> maks|>, ogr, rep1],
           10 + Length[odabrane]*(ogr + 5)*Max[rep1, 1]])],
      Dynamic[rezP]}, Spacings -> 1]],
    panel["Kanonska faktorizacija (PotpunaFaktorizacija)", Column[{
      Row[{poljeDugo[n2, "n"], Spacer[15], dugme["Faktoriziraj", rez2 = izlaz[rPotpuna[n2], 120]]}],
      Dynamic[rez2]}, Spacings -> 1]],
    panel["Mjerenje vremena u zavisnosti od bitovne du\[ZHacek]ine (n = p\[CenterDot]q, p i q su m-bitni prosti brojevi)", Column[{
      Row[{Column[{Style["Metode", Bold],
          CheckboxBar[Dynamic[odabrane3], imenaMetoda, Appearance -> "Vertical"]}],
        Spacer[25],
        Column[{polje[bMin, "m_min", 6], polje[bMax, "m_max", 6], polje[korak, "korak", 6],
          polje[ponav, "ponavljanja", 6]}, Spacings -> 0.5]}],
      objasni["Koriste se parametri i vremensko ograni\[CHacek]enje iz gornje kartice. Prikazuje se medijana vremena po bitovnoj du\[ZHacek]ini; mjeri se samo poziv funkcije paketa. Mjerenje mo\[ZHacek]e potrajati."],
      dugme["Izmjeri",
        rez3 = izlazBez[rMjerenje[odabrane3, bMin, bMax, korak, ponav,
          <|"Bmin" -> Bmin, "Bmax" -> Bmax, "a" -> aP, "x0" -> x0, "c" -> cR, "w" -> w, "maks" -> maks|>, ogr],
          600]],
      Dynamic[rez3]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Kartica: Elipticke krivulje                                         *)
(* ------------------------------------------------------------------ *)

rKrivulja[a_, b_, p_] := Which[
  !cijeliQ[{a, b, p}], neCijeli,
  p < 5 || !MillerRabinTest[p, 20], greska["p mora biti prost broj, p \[GreaterEqual] 5."],
  !ValidnaKrivulja[a, b, p], greska["Krivulja je singularna: 4a^3 + 27b^2 \[Congruent] 0 (mod p)."],
  True, Module[{valid, red, redR, gen, hasse, lo, hi, tacke},
    valid = ValidnaKrivulja[a, b, p];
    red = RedKrivuljeBruteForce[a, b, p];
    redR = RedKrivulje[a, b, p];
    hasse = HasseProvjera[p, red];
    gen = GeneratoriKrivulje[a, b, p];
    tacke = If[p <= 3000, tackeKrivulje[a, b, p], {}];
    lo = Ceiling[p + 1 - 2*Sqrt[p]];
    hi = Floor[p + 1 + 2*Sqrt[p]];
    dvaDijela[{izv["ValidnaKrivulja", {a, b, p}, valid], izv["RedKrivuljeBruteForce", {a, b, p}, red],
        izv["RedKrivulje", {a, b, p}, redR], izv["HasseProvjera", {p, red}, hasse],
        izv["GeneratoriKrivulje", {a, b, p}, gen]},
      Column[{
        tablica[{
          "Krivulja" -> zag["y^2 = x^3 + " <> ToString[a] <> "x + " <> ToString[b] <> " nad F_" <> ToString[p]],
          "Hasseov interval" -> {lo, hi},
          "Grupa je cikli\[CHacek]na" -> (gen =!= {}),
          "Broj generatora" -> Length[gen]}],
        tablicaVisekratnika[a, b, p],
        If[p <= 3000, Row[{grafKrivulje[a, b, p], realnaKrivulja[a, b]}, Spacer[10]],
          Style["(crtanje ta\[CHacek]aka samo za p \[LessEqual] 3000)", Gray]]
      }, Spacings -> 1]]
  ]
]

rTacke[a_, b_, p_, x1_, y1_, x2_, y2_, k_] := Which[
  !cijeliQ[{a, b, p, x1, y1, x2, y2, k}], neCijeli,
  p < 5 || !MillerRabinTest[p, 20], greska["p mora biti prost broj, p \[GreaterEqual] 5."],
  !ValidnaKrivulja[a, b, p], greska["Krivulja je singularna."],
  !TackaNaKrivulji[x1, y1, a, b, p], greska["Ta\[CHacek]ka P ne le\[ZHacek]i na krivulji."],
  !TackaNaKrivulji[x2, y2, a, b, p], greska["Ta\[CHacek]ka Q ne le\[ZHacek]i na krivulji."],
  k < 0, greska["Skalar k mora biti \[GreaterEqual] 0."],
  True, Module[{P, Q, zbir, dupl, kP, redP, orbita},
    P = {Mod[x1, p], Mod[y1, p]};
    Q = {Mod[x2, p], Mod[y2, p]};
    zbir = SabiranjeTacaka[P, Q, a, p];
    dupl = SabiranjeTacaka[P, P, a, p];
    kP = VisekratnikTacke[k, P, a, p];
    redP = RedTacke[P, a, p];
    orbita = If[p <= 3000, NestList[SabiranjeTacaka[#, P, a, p] &, P, redP - 2], {}];
    dvaDijela[{izv["TackaNaKrivulji", {P[[1]], P[[2]], a, b, p}, True],
        izv["SabiranjeTacaka", {P, Q, a, p}, zbir], izv["SabiranjeTacaka", {P, P, a, p}, dupl],
        izv["VisekratnikTacke", {k, P, a, p}, kP], izv["RedTacke", {P, a, p}, redP]},
      Column[{
        beskonacnoNapomene[P, Q, zbir, dupl, k, kP, redP],
        If[p <= 3000, podgrupaPrikaz[a, b, p, P, redP], objasni["(prikaz podgrupe samo za p \[LessEqual] 3000)"]],
        tablicaVisekratnika[a, b, p],
        If[P =!= Q, koraciSab["Koraci: P + Q", P, Q, a, p, zbir], Nothing],
        koraciSab["Koraci: 2P", P, P, a, p, dupl],
        koraciOtvori["Koraci: vi\[SHacek]ekratnik [k]P", koraciVisek[k, P, a, p, kP]],
        If[p <= 3000,
          Column[{
            grafKrivulje[a, b, p, orbita, {{Red, "P", P}, {Blue, "Q", Q},
              {Darker[Green], "P+Q", zbir}, {Magenta, "kP", kP}}],
            realnoSabiranje[a, b]}, Spacings -> 1],
          Style["(crtanje samo za p \[LessEqual] 3000)", Gray]]
      }, Spacings -> 1]]
  ]
]

rSM[a_, b_, p_, x_, y_] := Which[
  !cijeliQ[{a, b, p, x, y}], neCijeli,
  p < 5 || !MillerRabinTest[p, 20], greska["p mora biti prost broj."],
  !ValidnaKrivulja[a, b, p], greska["Krivulja je singularna."],
  !TackaNaKrivulji[x, y, a, b, p], greska["Ta\[CHacek]ka P ne le\[ZHacek]i na krivulji."],
  True, Module[{P, red, redR, lo, hi, ok, stavke},
    P = {Mod[x, p], Mod[y, p]};
    red = RedKrivuljeShanksMestre[a, b, p, P];
    redR = RedKrivulje[a, b, p, P];
    lo = Ceiling[p + 1 - 2*Sqrt[p]];
    hi = Floor[p + 1 + 2*Sqrt[p]];
    stavke = {izv["RedKrivuljeShanksMestre", {a, b, p, P}, red], izv["RedKrivulje", {a, b, p, P}, redR]};
    If[IntegerQ[red],
      ok = VisekratnikTacke[red, P, a, p];
      AppendTo[stavke, izv["HasseProvjera", {p, red}, HasseProvjera[p, red]]];
      AppendTo[stavke, izv["VisekratnikTacke", {red, P, a, p}, ok]];
      dvaDijela[stavke, Column[{
        tablica[{"Hasseov interval" -> {lo, hi}, "[#E]P = O" -> !ListQ[ok]}]
      }, Spacings -> 1]],
      dvaDijela[stavke, greska["Kandidat nije jedinstven. Poku\[SHacek]ajte s drugom ta\[CHacek]kom P."]]
    ]
  ]
]

tabEK[] := DynamicModule[{a = 2, b = 3, p = 97, x1 = 3, y1 = 6, x2 = 80, y2 = 10, k = 5,
  rez1 = "", rez2 = "",
  a3 = 2, b3 = 3, p3 = 1000003, x3 = 1, y3 = 586770, rez3 = ""},
  Column[{
    panel["Krivulja y^2 = x^3 + ax + b nad F_p: red, Hasseov teorem, generatori", Column[{
      Row[{polje[a, "a", 6], Spacer[12], polje[b, "b", 6], Spacer[12], polje[p, "p", 8], Spacer[12],
        dugme["Analiziraj krivulju", rez1 = izlaz[rKrivulja[a, b, p], 120]]}],
      Dynamic[rez1]}, Spacings -> 1]],
    panel["Ra\[CHacek]unanje s ta\[CHacek]kama: P + Q, 2P, kP, red ta\[CHacek]ke (koristi a, b, p iz gornjeg panela)", Column[{
      Row[{polje[x1, "x_P", 6], Spacer[8], polje[y1, "y_P", 6], Spacer[16],
        polje[x2, "x_Q", 6], Spacer[8], polje[y2, "y_Q", 6], Spacer[16], polje[k, "k", 6]}],
      dugme["Izra\[CHacek]unaj", rez2 = izlaz[rTacke[a, b, p, x1, y1, x2, y2, k], 120]],
      Dynamic[rez2]}, Spacings -> 1]],
    panel["Veliki p: red krivulje Shanks\[Dash]Mestreovom metodom", Column[{
      Row[{polje[a3, "a", 6], Spacer[12], polje[b3, "b", 6], Spacer[12], polje[p3, "p", 14]}],
      Row[{polje[x3, "x_P", 8], Spacer[12], polje[y3, "y_P", 8], Spacer[12],
        dugme["Izra\[CHacek]unaj red", rez3 = izlaz[rSM[a3, b3, p3, x3, y3], 180]]}],
      Dynamic[rez3]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Koraci racunanja: tabele koraka algoritama                          *)
(* Svaka funkcija tr... ponavlja korake algoritma iz paketa           *)
(* Kriptografija.wl (verzija bez Ispis funkcija) i biljezi ih u tabelu. *)
(* Konacni rezultat uvijek daje funkcija paketa; tabela se uporedjuje  *)
(* s njim gdje je algoritam deterministican.                          *)
(* ------------------------------------------------------------------ *)

$maxRedova = 35;

celija[x_Integer] := skrati[x]
celija[x_String] := zag[x]
celija[x_] := x

(* Oznake oblika r_i, r_{i-1}, x^2, a^(-1), a_j^k pisu se kao pravi indeksi i eksponenti *)
$zagRegex = "[A-Za-z0-9)\[Alpha]\[Beta]\[Lambda]\[Mu]](?=[_^])(?:_(?:\\{[^}]*\\}|[A-Za-z0-9]+))?(?:\\^(?:\\((?:[^()]|\\([^()]*\\))*\\)|\\{[^}]*\\}|[A-Za-z0-9]+|\\*))?";

pocinje[s_String, p_String] := StringLength[s] >= StringLength[p] && StringTake[s, StringLength[p]] === p

zagKutija[m_String] := Module[{b, rest, ind = "", eks = "", k, tijelo},
  b = StringTake[m, 1];
  rest = StringDrop[m, 1];
  If[pocinje[rest, "_"],
    If[pocinje[rest, "_{"],
      k = StringPosition[rest, "}"][[1, 1]];
      ind = StringTake[rest, {3, k - 1}];
      rest = StringDrop[rest, k],
      ind = StringTake[rest, {2, 2}];
      rest = StringDrop[rest, 2];
      While[rest =!= "" && StringMatchQ[StringTake[rest, 1], RegularExpression["[A-Za-z0-9]"]],
        ind = ind <> StringTake[rest, 1]; rest = StringDrop[rest, 1]]]];
  If[pocinje[rest, "^"],
    tijelo = StringDrop[rest, 1];
    eks = If[pocinje[tijelo, "("] || pocinje[tijelo, "{"], StringTake[tijelo, {2, -2}], tijelo]];
  DisplayForm[Which[
    ind =!= "" && eks =!= "", SubsuperscriptBox[b, ind, eks],
    ind =!= "", SubscriptBox[b, ind],
    True, SuperscriptBox[b, eks]]]
]

zag[s_String] := Module[{pos, prev = 0, out = {}},
  pos = StringPosition[s, RegularExpression[$zagRegex], Overlaps -> False];
  If[pos === {}, s,
    Do[
      If[q[[1]] > prev + 1, AppendTo[out, StringTake[s, {prev + 1, q[[1]] - 1}]]];
      AppendTo[out, zagKutija[StringTake[s, q]]];
      prev = q[[2]],
      {q, pos}];
    If[prev < StringLength[s], AppendTo[out, StringDrop[s, prev]]];
    Row[out]]
]
zag[x_] := x

koraciTabela[zagl_List, redovi_List, istakni_: None] := Module[{r, napomena = None, n = Length[redovi]},
  r = MapIndexed[
    Function[{red, ind}, If[ind[[1]] === istakni,
      Item[Style[celija[#], Bold], Background -> LightYellow] & /@ red,
      celija /@ red]], redovi];
  If[n > $maxRedova,
    napomena = "Prikazano je prvih 25 i posljednjih 10 od " <> ToString[n] <> " koraka.";
    r = Join[Take[r, 25], {Table["...", {Length[zagl]}]}, Take[r, -10]]];
  Column[{
    Grid[Prepend[r, Style[zag[#], Bold] & /@ zagl], Frame -> All, Alignment -> Left,
      Spacings -> {1.2, 0.6}, Background -> {{LightBlue, None}, {LightBlue, None}}],
    If[napomena =!= None, objasni[napomena], Nothing]}, Spacings -> 0.6]
]

pomocniNatpis = "Pomo\[CAcute]ni prora\[CHacek]un za demonstraciju. Kona\[CHacek]an rezultat daje funkcija paketa.";

koraci[naslov_String, tabela_, dodatak_: Nothing] := Column[{
  Style[zag[naslov], Bold, 13],
  Style[pomocniNatpis, 10, Gray, Italic],
  tabela, dodatak}, Spacings -> 0.8]

slaganje[tr_, paket_] := If[tr === paket,
  Style["Rezultat iz tabele jednak je izlazu funkcije paketa.", Darker[Green], 11],
  Style["UPOZORENJE: rezultat iz tabele razlikuje se od izlaza funkcije paketa.", Red, Bold, 11]]

(* Euklidov algoritam, kao EuklidovAlgoritam *)
trEuklid[a_Integer, b_Integer] := Module[{r, q, m, redovi = {}},
  r[0] = a; r[1] = b; m = 1;
  While[r[m] != 0,
    q[m] = Floor[r[m - 1]/r[m]];
    r[m + 1] = r[m - 1] - q[m]*r[m];
    AppendTo[redovi, {m, r[m - 1], r[m], q[m], r[m + 1]}];
    m++];
  m--;
  <|"redovi" -> redovi, "rezultat" -> {Table[q[i], {i, 1, m}], r[m]}|>
]

koraciEuklid[a_Integer, b_Integer] := Module[{t = trEuklid[a, b]},
  koraci["Koraci: Euklidov algoritam",
    koraciTabela[{"i", "r_{i-1}", "r_i", "q_i", "r_{i+1}"}, t["redovi"]],
    slaganje[t["rezultat"], EuklidovAlgoritam[a, b]]]
]

(* prosireni Euklidov algoritam, kao ProsireniEuklid *)
trProsireni[a_Integer, b_Integer] := Module[{r, x, y, q, i, k, redovi},
  r[-1] = a; r[0] = b;
  x[-1] = 1; x[0] = 0;
  y[-1] = 0; y[0] = 1;
  redovi = {{-1, "-", a, 1, 0}, {0, "-", b, 0, 1}};
  i = 1;
  While[r[i - 1] > 0,
    q[i] = Floor[r[i - 2]/r[i - 1]];
    r[i] = r[i - 2] - q[i]*r[i - 1];
    x[i] = x[i - 2] - q[i]*x[i - 1];
    y[i] = y[i - 2] - q[i]*y[i - 1];
    AppendTo[redovi, {i, q[i], r[i], x[i], y[i]}];
    i++];
  k = i - 2;
  <|"redovi" -> redovi, "k" -> k, "rezultat" -> {r[k], x[k], y[k]}|>
]

koraciProsireni[a_Integer, b_Integer] := Module[{t = trProsireni[a, b]},
  koraci["Koraci: pro\:0161ireni Euklidov algoritam",
    koraciTabela[{"i", "q_i", "r_i", "x_i", "y_i"}, t["redovi"], t["k"] + 2],
    Column[{objasni["\:017duto: red iz kojeg se \:010dita rezultat {nzd, x, y}."],
      slaganje[t["rezultat"], ProsireniEuklid[a, b]]}]]
]

(* multiplikativni inverz, kao MultiplikativniInverz *)
trInverz[a_Integer, n_Integer] := Module[{r, y, q, i, k, redovi},
  r[-1] = n; r[0] = a;
  y[-1] = 0; y[0] = 1;
  redovi = {{-1, "-", n, 0}, {0, "-", a, 1}};
  i = 1;
  While[r[i - 1] > 0,
    q[i] = Floor[r[i - 2]/r[i - 1]];
    r[i] = r[i - 2] - q[i]*r[i - 1];
    y[i] = y[i - 2] - q[i]*y[i - 1];
    AppendTo[redovi, {i, q[i], r[i], y[i]}];
    i++];
  k = i - 2;
  <|"redovi" -> redovi, "k" -> k, "rezultat" -> If[r[k] != 1, None, Mod[y[k], n]]|>
]

koraciInverz[a_Integer, n_Integer, paket_] := Module[{t = trInverz[a, n]},
  koraci["Koraci: multiplikativni inverz",
    koraciTabela[{"i", "q_i", "r_i", "y_i"}, t["redovi"], t["k"] + 2],
    Column[{objasni[zag["\:017duto: red iz kojeg se \:010dita y_k; inverz je y_k mod n."]],
      slaganje[t["rezultat"], paket]}]]
]

(* modularno potenciranje, kao ModPotenciranje *)
trModPot[a_Integer, n_Integer, m_Integer] := Module[{bitovi, s, y, y1, redovi = {}, e = 0},
  bitovi = IntegerDigits[n, 2];
  s = Length[bitovi];
  y = 1;
  Do[
    y1 = Mod[y^2, m];
    y = y1;
    If[bitovi[[k]] == 1, y = Mod[y*a, m]];
    e = 2*e + bitovi[[k]];
    AppendTo[redovi, {k, bitovi[[k]], e, y1, If[bitovi[[k]] == 1, y, "-"], y}],
    {k, 1, s}];
  <|"redovi" -> redovi, "rezultat" -> y|>
]

koraciModPot[a_Integer, n_Integer, m_Integer] := Module[{t = trModPot[a, n, m]},
  koraci["Koraci: kvadriraj i mno\:017ei",
    koraciTabela[{"k", "bit", "eksponent do k", "y^2 mod m", "y\[CenterDot]a mod m (ako je bit 1)", "y"}, t["redovi"]],
    Column[{objasni["Binarni zapis eksponenta: " <> StringJoin[ToString /@ IntegerDigits[n, 2]] <> ". Po\:010detno y = 1."],
      slaganje[t["rezultat"], ModPotenciranje[a, n, m]]}]]
]

(* konvergente, kao Konvergente *)
trKonv[kolicnici_List] := Module[{p, q, n, i},
  n = Length[kolicnici];
  p[-2] = 0; p[-1] = 1;
  q[-2] = 1; q[-1] = 0;
  Do[
    p[i] = kolicnici[[i + 1]]*p[i - 1] + p[i - 2];
    q[i] = kolicnici[[i + 1]]*q[i - 1] + q[i - 2],
    {i, 0, n - 1}];
  <|"redovi" -> Table[{i, kolicnici[[i + 1]], p[i], q[i], tri[N[p[i]/q[i]]]}, {i, 0, n - 1}],
    "rezultat" -> Table[{p[i], q[i]}, {i, 0, n - 1}]|>
]

koraciKonv[kolicnici_List] := Module[{t = trKonv[kolicnici]},
  koraci["Koraci: konvergente",
    koraciTabela[{"i", "a_i", "p_i", "q_i", "p_i/q_i"}, t["redovi"]],
    slaganje[t["rezultat"], Konvergente[kolicnici]]]
]

(* Kineski teorem o ostacima, kao KineskiTeoremOOstacima *)
trKTO[a_List, m_List] := Module[{M, x, r, j, n, u, redovi = {}},
  r = Length[m];
  M = 1;
  For[j = 1, j <= r, j++, M = M*m[[j]]];
  x = 0;
  For[j = 1, j <= r, j++,
    n = M/m[[j]];
    u = MultiplikativniInverz[Mod[n, m[[j]]], m[[j]]];
    x = x + a[[j]]*n*u;
    AppendTo[redovi, {j, a[[j]], m[[j]], n, u, a[[j]]*n*u}]];
  <|"redovi" -> redovi, "M" -> M, "suma" -> x, "rezultat" -> Mod[x, M]|>
]

koraciKTO[a_List, m_List] := Module[{t = trKTO[a, m]},
  koraci["Koraci: Kineski teorem o ostacima",
    koraciTabela[{"j", "a_j", "m_j", "n_j = M/m_j", "u_j = n_j^(-1) mod m_j", "a_j\[CenterDot]n_j\[CenterDot]u_j"}, t["redovi"]],
    Column[{objasni["M = " <> ToString[t["M"]] <> ",  zbir = " <> ToString[t["suma"]] <> ",  x = zbir mod M = " <> ToString[t["rezultat"]]],
      slaganje[t["rezultat"], KineskiTeoremOOstacima[a, m]]}]]
]

(* testovi prostosti, kao FermatovTest, SolovayStrassenTest i MillerRabinTest.
   SeedRandom se poziva jednom, prije petlje, kao u paketu. *)
trFermat[n_Integer, k_Integer] := Module[{b, r, i, redovi = {}, ishod = True},
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    r = ModPotenciranje[b, n - 1, n];
    AppendTo[redovi, {i, b, r, If[r != 1, "pada", "prolazi"]}];
    If[r != 1, ishod = False; Break[]];
    i++];
  <|"redovi" -> redovi, "rezultat" -> ishod|>
]

trSS[n_Integer, k_Integer] := Module[{b, r, s, i, redovi = {}, ishod = True},
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    r = ModPotenciranje[b, (n - 1)/2, n];
    If[r != 1 && r != n - 1,
      AppendTo[redovi, {i, b, r, "-", "pada"}]; ishod = False; Break[]];
    s = Mod[JacobijevSimbol[b, n], n];
    If[r != s,
      AppendTo[redovi, {i, b, r, s, "pada"}]; ishod = False; Break[]];
    AppendTo[redovi, {i, b, r, s, "prolazi"}];
    i++];
  <|"redovi" -> redovi, "rezultat" -> ishod|>
]

trMR[n_Integer, k_Integer] := Module[{s, t, i, b, y, j, niz, pada, razlog, redovi = {}, ishod = True},
  s = 0;
  t = n - 1;
  While[Mod[t, 2] == 0, t = t/2; s++];
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    y = ModPotenciranje[b, t, n];
    niz = {y}; pada = False; razlog = "";
    If[y != 1 && y != n - 1,
      j = 1;
      While[j < s && y != n - 1,
        y = ModPotenciranje[y, 2, n];
        AppendTo[niz, y];
        If[y == 1, pada = True; razlog = " (y = 1)"; Break[]];
        j++];
      If[!pada && y != n - 1, pada = True; razlog = " (n-1 nije dostignut)"]];
    AppendTo[redovi, {i, b, StringRiffle[skrati /@ niz, " \[Rule] "], If[pada, "pada" <> razlog, "prolazi"]}];
    If[pada, ishod = False; Break[]];
    i++];
  <|"s" -> s, "t" -> t, "redovi" -> redovi, "rezultat" -> ishod|>
]

koraciTestovi[n_Integer, k_Integer] := If[EvenQ[n] || n < 5,
  objasni["Tabele koraka prikazuju se za neparne n \[GreaterEqual] 5."],
  Module[{f = trFermat[n, k], ss = trSS[n, k], mr = trMR[n, k], zav},
    zav = Function[t, If[t["rezultat"],
      "svih " <> ToString[k] <> " rundi je pro\:0161lo: n je vjerovatno prost.",
      "n je sigurno slo\:017een."]];
    Column[{
      Style["Koraci: testovi prostosti", Bold, 13],
      objasni["Tabele prikazuju zasebno izvr\:0161avanje testova sa slu\:010dajnim bazama, pa se baze mogu razlikovati od onih u izlazu funkcija. Zato se ovdje ne poredi ishod, nego se prati tok svakog testa."],
      Style[zag["Fermatov test: b^(n-1) mod n mora biti 1"], Bold],
      koraciTabela[{"i", "b", "b^(n-1) mod n", "ishod runde"}, f["redovi"]],
      Style[zav[f], 11],
      Style[zag["Solovay\[Dash]Strassenov test: r = b^((n-1)/2) mod n mora biti 1 ili n-1 i jednak Jacobijevom simbolu (mod n)"], Bold],
      koraciTabela[{"i", "b", "r", "(b/n) mod n", "ishod runde"}, ss["redovi"]],
      Style[zav[ss], 11],
      Style[zag["Miller\[Dash]Rabinov test: n-1 = 2^" <> ToString[mr["s"]] <> " \[CenterDot] " <> ToString[mr["t"]]], Bold],
      koraciTabela[{"i", "b", "y_0 = b^t mod n, y_1, y_2, ...", "ishod runde"}, mr["redovi"]],
      Style[zav[mr], 11]}, Spacings -> 0.8]
  ]
]

(* Pollardova p-1 metoda, kao PollardovaPMinus1 *)
trPMinus1[n_Integer, Bmin_Integer, Bmax_Integer, a_Integer] := Module[{B, L, k, e, t, d, rez, redovi},
  B = Bmin;
  rez = "Neuspjeh. Pove\[CAcute]ati B i/ili promijeniti a.";
  redovi = Reap[While[B <= Bmax,
    L = EratostenovoSito[B];
    k = 1;
    Do[
      e = 0;
      While[p^(e + 1) <= B, e++];
      k = k*p^e,
      {p, L}];
    t = ModPotenciranje[a, k, n];
    d = nzd[t - 1, n];
    Sow[{B, k, t, d}];
    If[1 < d < n, rez = d; Break[]];
    B++]][[2]];
  <|"redovi" -> If[redovi === {}, {}, redovi[[1]]], "rezultat" -> rez|>
]

koraciPMinus1[n_Integer, Bmin_Integer, Bmax_Integer, a_Integer, paket_] :=
  If[Bmax - Bmin > 5000,
    objasni["Tabela koraka za Pollardovu p-1 metodu prikazuje se za najvi\:0161e 5001 vrijednost granice B."],
    Module[{t = trPMinus1[n, Bmin, Bmax, a]},
      koraci["Koraci: Pollardova p-1 metoda",
        koraciTabela[{"B", "k = \[Product] p^e (p^e \[LessEqual] B)", "t = a^k mod n", "nzd(t-1, n)"}, t["redovi"]],
        slaganje[t["rezultat"], paket]]]
  ]


koraciPMinus1Tijelo[n_Integer, Bmin_Integer, Bmax_Integer, a_Integer, paket_] :=
  If[Bmax - Bmin > 5000,
    objasni["Tabela koraka za Pollardovu p-1 metodu prikazuje se za najvi\[SHacek]e 5001 vrijednost granice B."],
    Module[{t = trPMinus1[n, Bmin, Bmax, a]},
      Column[{
        Style[pomocniNatpis, 10, Gray, Italic],
        koraciTabela[{"B", "k = \[Product] p^e (p^e \[LessEqual] B)", "t = a^k mod n", "nzd(t-1, n)"}, t["redovi"]],
        slaganje[t["rezultat"], paket]}, Spacings -> 0.6]]
  ]


(* ------------------------------------------------------------------ *)
(* Koraci racunanja, druga faza: faktorizacija, elipti\:010dke krivulje,    *)
(* formule kriptosistema s uvr\:0161tenim vrijednostima                     *)
(* ------------------------------------------------------------------ *)

u[x_Integer] := If[x < 0, "(" <> skrati[x] <> ")", skrati[x]]
u[x_] := ToString[x]

koraciOtvori[naslov_String, sadrzaj_, otvoreno_: False] :=
  OpenerView[{Style[zag[naslov], Bold, 13], sadrzaj}, otvoreno]

(* tabela s ogranicenim brojem redova: prvih 25 i posljednjih 10, uz ukupan broj koraka *)
koraciTabelaSkr[zagl_List, glava_List, rep_List, ukupno_Integer] := Module[{izostavljeno, r},
  izostavljeno = ukupno - Length[glava] - Length[rep];
  r = Join[Map[celija, glava, {2}],
    If[izostavljeno > 0, {Table["...", {Length[zagl]}]}, {}],
    Map[celija, rep, {2}]];
  Column[{
    Grid[Prepend[r, Style[zag[#], Bold] & /@ zagl], Frame -> All, Alignment -> Left,
      Spacings -> {1.2, 0.6}, Background -> {{LightBlue, None}, {LightBlue, None}}],
    If[izostavljeno > 0,
      objasni["Ukupno koraka: " <> ToString[ukupno] <> ". Prikazano je prvih " <> ToString[Length[glava]] <>
        " i posljednjih " <> ToString[Length[rep]] <> "."],
      Nothing]}, Spacings -> 0.6]
]

(* pomocna funkcija: biljezi redove, cuva prvih 25 i posljednjih 10 *)
SetAttributes[zabiljezi, HoldAll]
zabiljezi[{glava_, rep_, broj_}, red_] := (
  broj++;
  If[broj <= 25, AppendTo[glava, red],
    rep = If[Length[rep] >= 10, Append[Rest[rep], red], Append[rep, red]]];)

neuspjeh = "Neuspjeh. Promijeniti c i/ili x0.";

(* Fermatova faktorizacija, kao FermatovaFaktorizacija *)
trFermatF[n_Integer] := Module[{x0, y0, d, sy0, x, y, p, q, i = 0, glava = {}, rep = {}, broj = 0, ishod},
  x0 = Floor[Sqrt[n]] + 1;
  y0 = x0^2 - n;
  d = 1;
  While[True,
    i++;
    sy0 = Floor[Sqrt[y0]];
    If[sy0^2 == y0,
      zabiljezi[{glava, rep, broj}, {i, x0 + i - 1, y0, sy0, "kvadrat"}];
      x = Floor[Sqrt[n + y0]];
      y = Floor[Sqrt[y0]];
      q = x - y;
      p = x + y;
      ishod = {p, q}; Break[],
      zabiljezi[{glava, rep, broj}, {i, x0 + i - 1, y0, sy0, "nije kvadrat"}];
      y0 = y0 + 2*x0 + d;
      d = d + 2;
      If[Floor[Sqrt[y0]] >= n/2,
        ishod = "Nisu prona\:0111eni netrivijalni faktori."; Break[]]]];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> ishod|>
]

(* Pollardova rho metoda, kao PollardovaRho *)
trRho[n_Integer, x0_Integer, c_Integer] := Module[{x, k, l, p, glava = {}, rep = {}, broj = 0, ishod = None},
  x[0] = x0;
  k = 1;
  While[True,
    x[k] = Mod[x[k - 1]^2 + c, n];
    l = 0;
    While[l <= k - 1,
      p = nzd[x[k] - x[l], n];
      zabiljezi[{glava, rep, broj}, {k, l, x[k], x[l], p}];
      If[1 < p < n, ishod = p; Break[]];
      If[p == n, ishod = neuspjeh; Break[]];
      l++];
    If[ishod =!= None, Break[]];
    k++];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> ishod|>
]

(* Floydova varijanta, kao PollardovaRhoFloyd *)
trFloyd[n_Integer, x0_Integer, c_Integer] := Module[{x, y, p, i = 0, glava = {}, rep = {}, broj = 0, ishod},
  x = x0; y = x0;
  While[True,
    i++;
    x = Mod[x^2 + c, n];
    y = Mod[y^2 + c, n];
    y = Mod[y^2 + c, n];
    p = nzd[x - y, n];
    zabiljezi[{glava, rep, broj}, {i, x, y, p}];
    If[1 < p < n, ishod = p; Break[]];
    If[p == n, ishod = neuspjeh; Break[]]];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> ishod|>
]

(* Brentova varijanta, kao PollardovaRhoBrent (m = 32) *)
trBrent[n_Integer, x0_Integer, c_Integer, m_Integer : 32] :=
  Module[{y, r, q, G, x, k, ys, glava = {}, rep = {}, broj = 0},
  y = x0; r = 1; q = 1; G = 1;
  While[G == 1,
    x = y;
    Do[y = Mod[y^2 + c, n], {r}];
    k = 0;
    While[k <= r && G == 1,
      ys = y;
      Do[
        y = Mod[y^2 + c, n];
        q = Mod[q*Abs[x - y], n],
        {Min[m, r - k]}];
      G = nzd[q, n];
      zabiljezi[{glava, rep, broj}, {r, k, x, y, q, G}];
      k += m];
    r *= 2];
  If[G == n,
    While[True,
      ys = Mod[ys^2 + c, n];
      G = nzd[Abs[x - ys], n];
      zabiljezi[{glava, rep, broj}, {"povratak", "-", x, ys, "-", G}];
      If[G > 1, Break[]]]];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> If[G == n, neuspjeh, G]|>
]

(* probno dijeljenje, kao FaktorizacijaDijeljenjem *)
trDijeljenje[n0_Integer] := Module[{n = n0, k, dk, q, r, e, rezultat, glava = {}, rep = {}, broj = 0},
  k = 0;
  rezultat = {};
  While[n != 1,
    dk = If[k == 0, 2, 2*k + 1];
    q = Quotient[n, dk];
    r = Mod[n, dk];
    If[r == 0,
      e = 0;
      While[Mod[n, dk] == 0, n = n/dk; e++];
      zabiljezi[{glava, rep, broj}, {k, dk, q, r, "dijeli, eksponent " <> ToString[e]}];
      AppendTo[rezultat, {dk, e}],
      If[q > dk,
        zabiljezi[{glava, rep, broj}, {k, dk, q, r, "ne dijeli"}];
        k++,
        zabiljezi[{glava, rep, broj}, {k, dk, q, r, "q \[LessEqual] d_k: preostali n je prost"}];
        AppendTo[rezultat, {n, 1}];
        Break[]]]];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> rezultat|>
]

(* tabele za faktorizaciju: ime metode, trag, zaglavlje *)
koraciFakt[ime_String, n_Integer, par_Association, paket_, ogr_] := Module[{t, zagl, naslov, ogranicenje},
  ogranicenje = If[ogr > 0, ogr, Infinity];
  If[paket === "Prekora\[CHacek]eno vrijeme." || paket === "Gre\[SHacek]ka pri izvr\[SHacek]avanju.", Return[Nothing]];
  Switch[ime,
    "Probno dijeljenje",
      t = TimeConstrained[trDijeljenje[n], ogranicenje, $Failed];
      zagl = {"k", "d_k", "q = n div d_k", "r = n mod d_k", "korak"}; naslov = "Koraci: probno dijeljenje",
    "Fermat",
      t = TimeConstrained[trFermatF[n], ogranicenje, $Failed];
      zagl = {"i", "x", "x^2 - n", "\[LeftFloor]\[Sqrt](x^2 - n)\[RightFloor]", "je li x^2 - n kvadrat"}; naslov = "Koraci: Fermatova faktorizacija",
    "Pollard rho",
      If[!IntegerQ[par["x0"]] || !IntegerQ[par["c"]], Return[Nothing]];
      t = TimeConstrained[trRho[n, par["x0"], par["c"]], ogranicenje, $Failed];
      zagl = {"k", "l", "x_k", "x_l", "nzd(x_k - x_l, n)"}; naslov = "Koraci: Pollardova rho metoda",
    "Pollard rho (Floyd)",
      If[!IntegerQ[par["x0"]] || !IntegerQ[par["c"]], Return[Nothing]];
      t = TimeConstrained[trFloyd[n, par["x0"], par["c"]], ogranicenje, $Failed];
      zagl = {"i", "x_i", "y_i = x_(2i)", "nzd(x_i - y_i, n)"}; naslov = "Koraci: Pollardova rho metoda (Floyd)",
    "Pollard rho (Brent)",
      If[!IntegerQ[par["x0"]] || !IntegerQ[par["c"]], Return[Nothing]];
      t = TimeConstrained[trBrent[n, par["x0"], par["c"]], ogranicenje, $Failed];
      zagl = {"r", "k", "x", "y", "q", "nzd(q, n)"}; naslov = "Koraci: Pollardova rho metoda (Brent)",
    "Pollard p-1",
      If[!IntegerQ[par["Bmin"]] || !IntegerQ[par["Bmax"]] || !IntegerQ[par["a"]], Return[Nothing]];
      Return[koraciOtvori["Koraci: Pollardova p-1 metoda",
        koraciPMinus1Tijelo[n, par["Bmin"], par["Bmax"], par["a"], paket]]],
    _, Return[Nothing]];
  If[t === $Failed,
    koraciOtvori[naslov, objasni["Tabela koraka nije dovr\:0161ena u zadanom vremenskom ograni\:010denju."]],
    koraciOtvori[naslov, Column[{
      Style[pomocniNatpis, 10, Gray, Italic],
      koraciTabelaSkr[zagl, t["glava"], t["rep"], t["ukupno"]],
      slaganje[t["rezultat"], paket]}, Spacings -> 0.6]]]
]

(* sabiranje tacaka, kao SabiranjeTacaka (za tacke razlicite od O) *)
trSab[P_List, Q_List, a_Integer, p_Integer] := Module[{x1, y1, x2, y2, lambda, x3, y3, inv, redovi, dupl, brojnik},
  x1 = P[[1]]; y1 = P[[2]];
  x2 = Q[[1]]; y2 = Q[[2]];
  If[x1 == x2 && Mod[y1 + y2, p] == 0,
    Return[<|"redovi" -> {{"P + Q", "x_1 = x_2 i y_1 + y_2 \[Congruent] 0 (mod p)",
        "x_1 = " <> u[x1] <> ", y_1 + y_2 = " <> u[y1 + y2], "O (ta\[CHacek]ka u beskona\[CHacek]nosti)"}},
      "rezultat" -> "O"|>]];
  dupl = (x1 == x2 && y1 == y2);
  If[dupl,
    inv = MultiplikativniInverz[Mod[2*y1, p], p];
    lambda = Mod[(3*x1^2 + a)*inv, p];
    redovi = {
      {"(2y_1)^(-1) mod p", "(2y_1)^(-1) mod p", "(2\[CenterDot]" <> u[y1] <> ")^(-1) mod " <> u[p], inv},
      {"\[Lambda]", "(3x_1^2 + a)\[CenterDot](2y_1)^(-1) mod p",
        "(3\[CenterDot]" <> u[x1] <> "^2 + " <> u[a] <> ")\[CenterDot]" <> u[inv] <> " mod " <> u[p], lambda}},
    inv = MultiplikativniInverz[Mod[x2 - x1, p], p];
    lambda = Mod[(y2 - y1)*inv, p];
    redovi = {
      {"(x_2 - x_1)^(-1) mod p", "(x_2 - x_1)^(-1) mod p", "(" <> u[x2] <> " - " <> u[x1] <> ")^(-1) mod " <> u[p], inv},
      {"\[Lambda]", "(y_2 - y_1)\[CenterDot](x_2 - x_1)^(-1) mod p",
        "(" <> u[y2] <> " - " <> u[y1] <> ")\[CenterDot]" <> u[inv] <> " mod " <> u[p], lambda}}];
  x3 = Mod[lambda^2 - x1 - x2, p];
  y3 = Mod[lambda*(x1 - x3) - y1, p];
  AppendTo[redovi, {"x_3", "\[Lambda]^2 - x_1 - x_2 mod p",
    u[lambda] <> "^2 - " <> u[x1] <> " - " <> u[x2] <> " mod " <> u[p], x3}];
  AppendTo[redovi, {"y_3", "\[Lambda](x_1 - x_3) - y_1 mod p",
    u[lambda] <> "\[CenterDot](" <> u[x1] <> " - " <> u[x3] <> ") - " <> u[y1] <> " mod " <> u[p], y3}];
  <|"redovi" -> redovi, "rezultat" -> {x3, y3}|>
]

koraciSab[naslov_String, P_List, Q_List, a_Integer, p_Integer, paket_] := Module[{t = trSab[P, Q, a, p]},
  koraci[naslov,
    koraciTabela[{"Veli\[CHacek]ina", "Formula", "Uvr\[SHacek]tene vrijednosti", "Vrijednost"}, t["redovi"]],
    slaganje[t["rezultat"], If[ListQ[paket], paket, "O"]]]
]

(* visekratnik tacke, kao VisekratnikTacke *)
trVisek[k_Integer, P_, a_Integer, p_Integer] := Module[{bitovi, Q, j = 0, glava = {}, rep = {}, broj = 0, Q2},
  bitovi = IntegerDigits[k, 2];
  Q = SabiranjeTacaka[{0, 1}, {0, p - 1}, a, p]; (* ta\[CHacek]ka O iz paketa *)
  Do[
    j++;
    Q = SabiranjeTacaka[Q, Q, a, p];
    Q2 = Q;
    If[bit == 1, Q = SabiranjeTacaka[Q, P, a, p]];
    zabiljezi[{glava, rep, broj}, {j, bit, prikaziTacku[Q2], If[bit == 1, prikaziTacku[Q], "-"]}],
    {bit, bitovi}];
  <|"glava" -> glava, "rep" -> rep, "ukupno" -> broj, "rezultat" -> Q|>
]

koraciVisek[k_Integer, P_List, a_Integer, p_Integer, paket_] := Module[{t = trVisek[k, P, a, p]},
  koraci["Koraci: vi\[SHacek]ekratnik ta\[CHacek]ke (kvadriraj i dodaj)",
    koraciTabelaSkr[{"j", "bit", "2Q", "2Q + P (ako je bit 1)"}, t["glava"], t["rep"], t["ukupno"]],
    Column[{objasni["Po\[CHacek]etno Q = O, bitovi od k = " <> StringJoin[ToString /@ IntegerDigits[k, 2]] <> "."],
      slaganje[If[ListQ[t["rezultat"]], t["rezultat"], "O"], If[ListQ[paket], paket, "O"]]}]]
]

(* tabela formula s uvrstenim vrijednostima *)
formule[redovi_List] :=
  koraciTabela[{"Veli\[CHacek]ina", "Formula", "Uvr\[SHacek]tene vrijednosti", "Vrijednost"}, redovi]

(* RSA *)
koraciRSAKljuc[n_, e_, d_, p_, q_] := Module[{phi = (p - 1)*(q - 1)},
  koraci["Koraci: provjera RSA klju\[CHacek]eva",
    formule[{
      {"n", "p\[CenterDot]q", u[p] <> "\[CenterDot]" <> u[q], p*q},
      {"\[Phi](n)", "(p - 1)(q - 1)", "(" <> u[p] <> " - 1)(" <> u[q] <> " - 1)", phi},
      {"e\[CenterDot]d mod \[Phi](n)", "e\[CenterDot]d mod \[Phi](n) = 1", u[e] <> "\[CenterDot]" <> u[d] <> " mod " <> u[phi], Mod[e*d, phi]},
      {"nzd(e, \[Phi](n))", "nzd(e, \[Phi](n)) = 1", "nzd(" <> u[e] <> ", " <> u[phi] <> ")", nzd[e, phi]}}]]
]

(* RSA \:0161ifriranje i de\:0161ifriranje: samo (sklopljena) tabela potenciranja, bez tabele formule *)
koraciRSASif[x_, n_, e_, y_] :=
  koraciOtvori["Koraci: x^e mod n (kvadriraj i mno\[ZHacek]i)", koraciModPot[x, e, n]]

koraciRSADes[y_, n_, d_, xd_] :=
  koraciOtvori["Koraci: y^d mod n (kvadriraj i mno\[ZHacek]i)", koraciModPot[y, d, n]]

(* Rabin *)
trRabinDes[c_Integer, p_Integer, q_Integer] := Module[{n, gab, a, b, r, s, x, y, redovi},
  n = p*q;
  gab = ProsireniEuklid[p, q];
  a = gab[[2]]; b = gab[[3]];
  r = ModPotenciranje[c, (p + 1)/4, p];
  s = ModPotenciranje[c, (q + 1)/4, q];
  x = Mod[a*p*s + b*q*r, n];
  y = Mod[a*p*s - b*q*r, n];
  <|"redovi" -> {
      {"a, b", "a\[CenterDot]p + b\[CenterDot]q = 1 (pro\:0161ireni Euklid)", "ProsireniEuklid(" <> u[p] <> ", " <> u[q] <> ")",
        "a = " <> u[a] <> ", b = " <> u[b]},
      {"r", "c^((p+1)/4) mod p", u[c] <> "^((" <> u[p] <> "+1)/4) mod " <> u[p], r},
      {"s", "c^((q+1)/4) mod q", u[c] <> "^((" <> u[q] <> "+1)/4) mod " <> u[q], s},
      {"x", "a\[CenterDot]p\[CenterDot]s + b\[CenterDot]q\[CenterDot]r mod n",
        u[a] <> "\[CenterDot]" <> u[p] <> "\[CenterDot]" <> u[s] <> " + " <> u[b] <> "\[CenterDot]" <> u[q] <> "\[CenterDot]" <> u[r] <> " mod " <> u[n], x},
      {"-x mod n", "n - x", u[n] <> " - " <> u[x], Mod[-x, n]},
      {"y", "a\[CenterDot]p\[CenterDot]s - b\[CenterDot]q\[CenterDot]r mod n",
        u[a] <> "\[CenterDot]" <> u[p] <> "\[CenterDot]" <> u[s] <> " - " <> u[b] <> "\[CenterDot]" <> u[q] <> "\[CenterDot]" <> u[r] <> " mod " <> u[n], y},
      {"-y mod n", "n - y", u[n] <> " - " <> u[y], Mod[-y, n]}},
    "rezultat" -> {x, Mod[-x, n], y, Mod[-y, n]}|>
]

koraciRabinDes[c_, p_, q_, paket_] := Module[{t = trRabinDes[c, p, q]},
  koraci["Koraci: Rabinovo de\[SHacek]ifriranje",
    formule[t["redovi"]],
    slaganje[t["rezultat"], paket]]
]

(* ElGamal: samo de\:0161ifriranje *)
koraciEGDes[y1_, y2_, p_, a_, xd_] := Module[{t, inv, xr},
  t = ModPotenciranje[y1, a, p];
  inv = MultiplikativniInverz[t, p];
  xr = Mod[y2*inv, p];
  koraci["Koraci: ElGamalovo de\[SHacek]ifriranje",
    formule[{
      {"y_1^a mod p", "y_1^a mod p", u[y1] <> "^" <> u[a] <> " mod " <> u[p], t},
      {"(y_1^a)^(-1) mod p", "(y_1^a)^(-1) mod p", "(" <> u[t] <> ")^(-1) mod " <> u[p], inv},
      {"x", "y_2\[CenterDot](y_1^a)^(-1) mod p", u[y2] <> "\[CenterDot]" <> u[inv] <> " mod " <> u[p], xr}}],
    slaganje[xr, xd]]
]

koraciMVDes[y0_List, y1_, y2_, a_, p_, mT_, paket_] := Module[{c, c1, c2, i1, i2, x1, x2},
  c = VisekratnikTacke[mT, y0, a, p];
  c1 = c[[1]]; c2 = c[[2]];
  i1 = MultiplikativniInverz[c1, p];
  i2 = MultiplikativniInverz[c2, p];
  x1 = Mod[y1*i1, p];
  x2 = Mod[y2*i2, p];
  Column[{
    koraci["Koraci: Menezes\[Dash]Vanstoneovo de\[SHacek]ifriranje",
      formule[{
        {"(c_1, c_2)", "[m]y_0", "[" <> u[mT] <> "]" <> ToString[y0], ToString[c]},
        {"c_1^(-1) mod p", "c_1^(-1) mod p", "(" <> u[c1] <> ")^(-1) mod " <> u[p], i1},
        {"x_1", "y_1\[CenterDot]c_1^(-1) mod p", u[y1] <> "\[CenterDot]" <> u[i1] <> " mod " <> u[p], x1},
        {"c_2^(-1) mod p", "c_2^(-1) mod p", "(" <> u[c2] <> ")^(-1) mod " <> u[p], i2},
        {"x_2", "y_2\[CenterDot]c_2^(-1) mod p", u[y2] <> "\[CenterDot]" <> u[i2] <> " mod " <> u[p], x2}}],
      slaganje[{x1, x2}, paket]],
    koraciOtvori["Koraci za [m]y_0", koraciVisek[mT, y0, a, p, c]]}, Spacings -> 0.8]
]
(* ------------------------------------------------------------------ *)
(* Dodatne vizualizacije (pomo\:0107ni prora\:010duni; rezultat uvijek daje      *)
(* funkcija paketa): cikli\:010dka podgrupa ta\:010dke, grupa Z_p*   *)
(* na kru\:017enici DH, KTO na brojevnoj pravi, primjeri strukture broja.   *)
(* ------------------------------------------------------------------ *)

kljucTablica[stavke_List] := tablica[Map[jt[#[[1]], #[[2]]] -> #[[3]] &, stavke]]

(* ---------------- Elipti\:010dke krivulje: cikli\:010dka podgrupa ---------------- *)

(* [1]P, [2]P, ..., [r]P = O; ta\:010dke ra\:010duna SabiranjeTacaka, red daje RedTacke *)
podgrupaPrikaz[a_Integer, b_Integer, p_Integer, P_List, r_Integer] := Module[{tacke, idx, oznake, strelice, redovi, redE, cijela},
  tacke = NestList[SabiranjeTacaka[#, P, a, p] &, P, Max[r - 1, 0]];
  idx = If[r <= 12, Range[1, r - 1], Join[Range[1, 6], Range[r - 3, r - 1]]];
  oznake = Table[{If[j == 1, Red, Blue], "[" <> ToString[j] <> "]P", tacke[[j]]}, {j, idx}];
  strelice = Table[{GrayLevel[0.3], Arrowheads[0.03], Arrow[{tacke[[j]], tacke[[j + 1]]}]},
    {j, Select[idx, MemberQ[idx, # + 1] && # + 1 <= r - 1 &]}];
  redE = RedKrivuljeBruteForce[a, b, p];
  cijela = (r == redE);
  redovi = Join[Table[{j, ToString[tacke[[j]]]}, {j, idx}], {{r, "O (ta\[CHacek]ka u beskona\[CHacek]nosti)"}}];
  koraci["Cikli\[CHacek]ka podgrupa generisana ta\[CHacek]kom P: P, [2]P, ..., [r]P = O",
    Column[{
      tablica[{"Red ta\[CHacek]ke r (RedTacke)" -> r, "Red krivulje #E" -> redE,
        "r dijeli #E (Lagrange)" -> Divisible[redE, r],
        "P generi\[SHacek]e cijelu grupu" -> cijela}],
      Show[grafKrivulje[a, b, p, Take[tacke, Max[r - 1, 0]], oznake], Graphics[strelice]],
      koraciTabela[{"j", "[j]P"}, redovi]}, Spacings -> 0.8]]
]


(* ---------------- Diffie-Hellman: grupa Z_p* na kru\:017enici ---------------- *)

dhKruznica[p_Integer, g_Integer, A_Integer, B_Integer, K_Integer] := Module[{R, poz, vals = {}, j = 1, v, ord, niz, strel, podgr, boja},
  R = Max[2.5, (p - 1)*1.1/(2 Pi)];
  poz[e_] := R*{Cos[Pi/2 - 2 Pi (e - 1)/(p - 1)], Sin[Pi/2 - 2 Pi (e - 1)/(p - 1)]};
  While[j <= p,
    v = ModPotenciranje[g, j, p];
    AppendTo[vals, v];
    If[v == 1, Break[]];
    j++];
  ord = Length[vals];
  niz = Prepend[vals, 1];
  strel = If[ord > 1,
    Table[Module[{p1 = poz[niz[[i]]], p2 = poz[niz[[i + 1]]], dd, u},
      dd = p2 - p1; u = dd/Max[Norm[dd], 10^-6];
      Arrow[{p1 + 0.4*u, p2 - 0.4*u}]], {i, 1, Length[niz] - 1}], {}];
  podgr = Union[vals];
  boja[e_] := Which[e == K, RGBColor[1, 0.5, 0.5], e == A, RGBColor[0.6, 0.75, 1], e == B, RGBColor[0.6, 0.9, 0.6],
    MemberQ[podgr, e], RGBColor[1, 0.9, 0.7], True, GrayLevel[0.95]];
  koraci["Grupa Z_p^* na kru\:017enici i stepeni g, g^2, g^3, ... (mod p)",
    Column[{
      tablica[{"Red elementa g (broj razli\[CHacek]itih stepena)" -> ord,
        "g je generator grupe Z_p^*" -> (ord == p - 1)}],
      Graphics[{
        {GrayLevel[0.4], Arrowheads[0.02], Opacity[0.7], strel},
        Table[{boja[e], EdgeForm[If[e == 1, {Thick, Black}, GrayLevel[0.5]]], Disk[poz[e], 0.45],
          Black, Text[Style[ToString[e], 9], poz[e]]}, {e, 1, p - 1}]},
        ImageSize -> Min[700, Max[320, 40 + 30*R]], PlotRange -> All, PlotRangePadding -> 1]}, Spacings -> 0.8]]
]


(* ---------------- KTO na brojevnoj pravi ---------------- *)

ktoPrava[a_List, m_List, x_Integer] := Module[{M = Times @@ m, s = Length[m], x0, boje, korak},
  x0 = Mod[x, M];
  korak = If[M <= 60, 1, 5];
  boje = {RGBColor[0.55, 0.75, 1], RGBColor[0.6, 0.9, 0.6], RGBColor[1, 0.8, 0.5],
    RGBColor[0.9, 0.7, 0.95], RGBColor[0.85, 0.85, 0.5]};
  koraci["Kongruencije na brojevnoj pravi (0, 1, ..., M - 1)",
    Graphics[{
      EdgeForm[GrayLevel[0.6]],
      Table[{
        Table[{If[Mod[v, m[[j]]] == Mod[a[[j]], m[[j]]], boje[[Mod[j - 1, 5] + 1]], White],
          Rectangle[{v, -j}, {v + 1, -j + 0.8}]}, {v, 0, M - 1}],
        Text[Style["x \[Congruent] " <> ToString[a[[j]]] <> " (mod " <> ToString[m[[j]]] <> ")", 11], {-0.3, -j + 0.4}, {1, 0}]},
        {j, 1, s}],
      Table[{If[v == x0, Red, White], Rectangle[{v, -s - 1}, {v + 1, -s - 0.2}]}, {v, 0, M - 1}],
      Text[Style["x \[Congruent] " <> ToString[x0] <> " (mod " <> ToString[M] <> ")", 11, Bold], {-0.3, -s - 0.6}, {1, 0}],
      {Red, Dashed, Line[{{x0 + 0.5, 0.9}, {x0 + 0.5, -s - 1.2}}]},
      Table[Text[Style[ToString[v], 8], {v + 0.5, 0.45}], {v, 0, M - 1, korak}]},
      ImageSize -> {Min[980, 190 + 14*M], 34 (s + 2) + 40}, AspectRatio -> Full, PlotRange -> {{0, M}, {-s - 1.3, 1}},
      PlotRangeClipping -> False, ImagePadding -> {{190, 10}, {10, 20}}]]
]


(* ---------------- Primjeri strukture broja (pore\:0111enje metoda faktorizacije) ---------------- *)
(* Brojevi su iste veli\:010dine (53 do 54 bita, oko 9\[CenterDot]10^15). Odabrani su i provjereni funkcijama paketa. *)

primjeriStrukture = <|
  "A" -> <|"n" -> 9025193040022231,
    "opis" -> "A: dva bliska prosta faktora, p = 95000011 i q = 95002021 (razlika 2010). Pogodno za Fermatovu metodu: x = (p + q)/2 je ve\[CAcute] prva ispitana vrijednost (prvi cijeli broj iznad korijena iz n).",
    "metode" -> {"Fermat", "Probno dijeljenje", "Pollard rho (Brent)", "Pollard p-1"}, "Bmin" -> 10, "Bmax" -> 2000, "ogr" -> 20|>,
  "B" -> <|"n" -> 8589960378803827,
    "opis" -> "B: jedan mali i jedan veliki prosti faktor, p = 1000003 i q = 8589934609. Pogodno za probno dijeljenje: mali faktor se nalazi poslije oko 5\[CenterDot]10^5 koraka, a Fermatova metoda treba oko 10^11 koraka.",
    "metode" -> {"Fermat", "Probno dijeljenje", "Pollard rho (Brent)", "Pollard p-1"}, "Bmin" -> 10, "Bmax" -> 2000, "ogr" -> 20|>,
  "C" -> <|"n" -> 8680515600909007,
    "opis" -> "C: p = 43402481 s glatkim p - 1 = 2^4\[CenterDot]5\[CenterDot]11\[CenterDot]31\[CenterDot]37\[CenterDot]43 i sigurni prost broj q = 200000447 (q - 1 = 2\[CenterDot]100000223). Pogodno za Pollardovu p-1 metodu: uspjeh zavisi od faktorizacije p - 1 i granice B (ovdje B = 43), a ne od toga \[SHacek]to je faktor mali.",
    "metode" -> {"Fermat", "Probno dijeljenje", "Pollard rho (Brent)", "Pollard p-1"}, "Bmin" -> 10, "Bmax" -> 2000, "ogr" -> 20|>
|>

(* ---------- Geometrijsko sabiranje tacaka na krivulji nad R (kao Slika 1.3 u radu) ---------- *)
realnoSabiranje[a_, b_] := Module[{kor, r1, r2, rmax, rmin, s, xP, xQ, yP, yQ, f, sek, tan, panel},
  If[!(NumericQ[a] && NumericQ[b]) || Abs[a] > 50 || Abs[b] > 50 || 4 a^3 + 27 b^2 == 0, Return[Nothing]];
  kor = Sort[Re /@ Select[x /. NSolve[x^3 + a x + b == 0, x], Abs[Im[#]] < 10^-8 &]];
  rmin = First[kor]; rmax = Last[kor];
  s = Max[1., Sqrt[Abs[a]] + 0.5];
  f[x_] := x^3 + a x + b;
  If[Length[kor] == 3,
    xP = kor[[1]] + 0.35 (kor[[2]] - kor[[1]]); xQ = rmax + s,
    xP = rmax + 0.5 s; xQ = rmax + 1.5 s];
  yP = Sqrt[f[xP]]; yQ = -Sqrt[f[xQ]];
  sek = Module[{l, x3, y3}, l = (yQ - yP)/(xQ - xP); x3 = l^2 - xP - xQ; y3 = yP + l (x3 - xP);
    <|"l" -> l, "R" -> {x3, y3}, "S" -> {x3, -y3}|>];
  tan = Module[{l, x3, y3}, l = (3 xP^2 + a)/(2 yP); x3 = l^2 - 2 xP; y3 = yP + l (x3 - xP);
    <|"l" -> l, "R" -> {x3, y3}, "S" -> {x3, -y3}|>];
  panel[tacke_List, pravac_Association, oznake_List, natpis_] := Module[{sve, xl, xd, yg, yd, xs, ys, kriva},
    sve = Join[tacke, {pravac["R"], pravac["S"]}];
    xl = Min[rmin, Min[sve[[All, 1]]]] - 0.6; xd = Max[rmax + s, Max[sve[[All, 1]]]] + 0.8;
    yg = 1.15 Max[Abs[sve[[All, 2]]]]; yd = -yg;
    Show[
      ContourPlot[y^2 == f[x], {x, xl, xd}, {y, yd, yg}, ContourStyle -> Black,
        Axes -> True, AxesStyle -> Arrowheads[0.03], Ticks -> None, Frame -> False,
        AxesLabel -> {Style["x", Italic], Style["y", Italic]}, PlotRange -> {{xl, xd}, {yd, yg}}],
      Graphics[{
        Thick, Line[{{xl, tacke[[1, 2]] + pravac["l"] (xl - tacke[[1, 1]])}, {xd, tacke[[1, 2]] + pravac["l"] (xd - tacke[[1, 1]])}}],
        Dotted, Line[{pravac["R"], pravac["S"]}],
        PointSize[0.018], Black, Point[Join[tacke, {pravac["R"], pravac["S"]}]],
        Sequence @@ MapThread[Text[Style[#1, Italic, 14], #2, #3] &, {oznake, Join[tacke, {pravac["R"], pravac["S"]}], {{-1.4, -0.9}, {-1.4, -0.9}, {-1.4, -0.9}, {-1.4, -0.9}}[[1 ;; Length[oznake]]]}]
      }],
      PlotRangeClipping -> False, ImagePadding -> {{15, 45}, {15, 25}},
      ImageSize -> 340, PlotLabel -> Style[natpis, 12]]];
  Column[{
    Row[{
      panel[{{xP, yP}, {xQ, yQ}}, sek,
        {"P", "Q", Row[{"P", "*", "Q"}], Row[{"P", "+", "Q"}]}, "P \[NotEqual] Q (sekanta)"],
      panel[{{xP, yP}}, tan,
        {"P", Row[{"P", "*", "P"}], "[2]P"}, "P = Q (tangenta)"]}, Spacer[10]]
  }, Spacings -> 1]
]

(* ---------- Tablica visekratnika [k]P svih tacaka (kao u radu) ---------- *)
tablicaVisekratnika[a_, b_, p_] := Module[{tacke, N0, oblik, zagl, redovi},
  If[!(IntegerQ[a] && IntegerQ[b] && IntegerQ[p]) || p > 100, Return[Nothing]];
  N0 = RedKrivuljeBruteForce[a, b, p];
  If[N0 > 16 || N0 < 2, Return[Nothing]];
  tacke = tackeKrivulje[a, b, p];
  oblik[T_] := If[!ListQ[T], Style["\[ScriptCapitalO]", 14], "(" <> ToString[T[[1]]] <> ", " <> ToString[T[[2]]] <> ")"];
  zagl = Join[{Style["P", Italic]}, Table[Row[{"[", k, "]", Style["P", Italic]}], {k, 2, N0}], {"red od P"}];
  redovi = Table[
    Join[{oblik[P0]}, Table[oblik[VisekratnikTacke[k, P0, a, p]], {k, 2, N0}], {RedTacke[P0, a, p]}],
    {P0, tacke}];
  Column[{
    Style["Tabela vi\[SHacek]ekratnika [k]P svih ta\[CHacek]aka krivulje", Bold, 13],
    Grid[Prepend[redovi, zagl], Frame -> All, Alignment -> Center, Spacings -> {1.2, 0.8}]
  }, Spacings -> 1]
]

(* ---------- Isticanje rezultata u beskonacnosti (tacka O) ---------- *)
beskonacnoNapomene[P_, Q_, zbir_, dupl_, k_, kP_, redP_] := Module[{l = {}},
  If[!ListQ[zbir],
    AppendTo[l, If[P === Q,
      "P + Q = \[ScriptCapitalO] (ta\[CHacek]ka u beskona\[CHacek]nosti): P = Q i y_P = 0, tangenta u P je vertikalna.",
      "P + Q = \[ScriptCapitalO] (ta\[CHacek]ka u beskona\[CHacek]nosti): Q = -P, ta\[CHacek]ke imaju istu x-koordinatu, a y_P + y_Q \[Congruent] 0 (mod p), pa je sekanta vertikalna."]]];
  If[!ListQ[dupl],
    AppendTo[l, "2P = \[ScriptCapitalO] (ta\[CHacek]ka u beskona\[CHacek]nosti): y_P = 0, pa je tangenta u P vertikalna. Red ta\[CHacek]ke P je 2."]];
  If[!ListQ[kP],
    AppendTo[l, If[k == 0,
      "[0]P = \[ScriptCapitalO] po definiciji.",
      "[" <> ToString[k] <> "]P = \[ScriptCapitalO] (ta\[CHacek]ka u beskona\[CHacek]nosti): red ta\[CHacek]ke P je " <> ToString[redP] <> ", a k = " <> ToString[k] <> " je njegov vi\[SHacek]ekratnik."]]];
  If[l === {}, Nothing,
    Framed[Column[Prepend[pasus[#, 12, Black, 90] & /@ l, Style["Rezultat u beskona\[CHacek]nosti", Bold, 13]], Spacings -> 0.8],
      Background -> RGBColor[1, 0.96, 0.8], FrameStyle -> RGBColor[0.85, 0.65, 0.1], RoundingRadius -> 4, FrameMargins -> 10]]
]



(* ------------------------------------------------------------------ *)
(* Zajednicke funkcije za kriptosisteme                                *)
(* ------------------------------------------------------------------ *)

ispisProvjere[r_] := If[r === True,
  Style["Svi uslovi su zadovoljeni: klju\[CHacek]evi su ispravni.", Darker[Green], Bold],
  Column[Prepend[Style[#, Red] & /@ If[ListQ[r], r, {ToString[r]}],
    Style["Uo\[CHacek]eni problemi:", Red, Bold]]]
]


(* ------------------------------------------------------------------ *)
(* Kartica: RSA                                                        *)
(* ------------------------------------------------------------------ *)

rsaGen[m_] := If[!IntegerQ[m] || m < 10,
  greska["Bitovna du\[ZHacek]ina m mora biti \[GreaterEqual] 10 (eksponent e bira se iz (10^5, \[Phi](n)-1))."],
  With[{r = RSAGenerisiKljuceve[m]}, bbsPromijenjen["RSAGenerisiKljuceve"]; r]
]

rsaPrikaz[n_, e_, d_, p_, q_] := Column[{
  tablica[{jt["p", "TAJNO"] -> p, jt["q", "TAJNO"] -> q, jt["n = p\[CenterDot]q", "JAVNO"] -> n,
    jt["\[Phi](n) = (p-1)(q-1)", "TAJNO"] -> (p - 1)*(q - 1),
    jt["e", "JAVNO"] -> e, jt["d", "TAJNO"] -> d, "Bitovna du\[ZHacek]ina n" -> IntegerLength[n, 2]}],
  koraciRSAKljuc[n, e, d, p, q]
}, Spacings -> 1]

rsaProvjera[n_, e_, d_, p_, q_] := If[!cijeliQ[{n, e, d, p, q}] || p < 2 || q < 2,
  neCijeli,
  provjeraPrikaz["RSAProvjeriKljuceve", {n, e, d, p, q}, RSAProvjeriKljuceve[n, e, d, p, q],
    If[(p - 1)*(q - 1) > 1, koraciRSAKljuc[n, e, d, p, q], Nothing]]
]

rsaSif[x_, n_, e_] := Which[
  !cijeliQ[{x, n, e}], neCijeli,
  n < 2 || e < 1, greska["Potrebno je n \[GreaterEqual] 2 i e \[GreaterEqual] 1."],
  x < 0 || x >= n, greska["Poruka mora zadovoljavati 0 \[LessEqual] x < n (za du\[ZHacek]e poruke potreban je ve\[CAcute]i n)."],
  True, RSASifriraj[x, n, e]
]

rsaDes[y_, n_, d_] := Which[
  !cijeliQ[{y, n, d}], neCijeli,
  n < 2 || d < 1, greska["Potrebno je n \[GreaterEqual] 2 i d \[GreaterEqual] 1."],
  y < 0 || y >= n, greska["\[CapitalSHacek]ifrat mora zadovoljavati 0 \[LessEqual] y < n."],
  True, RSADesifriraj[y, n, d]
]

rsaShema[x_, y_, xd_] := shema[
  "Alice (po\[SHacek]iljalac)", {"javni klju\[CHacek] (n, e)", "x = " <> skrati[x], "y = x^e mod n"},
  "Bob (primalac)", {"tajni klju\[CHacek] d", "x' = y^d mod n", If[xd === None, "", "x' = " <> skrati[xd]]},
  {{"->", "y = " <> skrati[y]}}]

tabRSA[] := DynamicModule[{m = 32, n = 3233, e = 17, d = 2753, p = 61, q = 53,
  x = 65, y = 2790, r, kl, rezKljuc = "", rezPoruka = ""},
  Column[{
    panel["Klju\[CHacek]evi", Column[{
      Row[{polje[m, "bitovna du\[ZHacek]ina prostih brojeva m"], Spacer[15],
        dugme["Generi\[SHacek]i klju\[CHacek]eve",
          (kl = izlaz[rsaGen[m]];
           If[ListQ[kl],
             {n, e, d, p, q} = kl;
             rezKljuc = dvaDijela[{izv["RSAGenerisiKljuceve", {m}, kl]}, Column[{rsaPrikaz[n, e, d, p, q], jtLegenda, slucajnaNapomena}, Spacings -> 1]],
             rezKljuc = kl])]}],
      objasni["Mo\[ZHacek]ete i ru\[CHacek]no upisati klju\[CHacek]eve (zadani su mali brojevi iz primjera)."],
      Row[{poljeDugo[p, jt["p", "TAJNO"]], Spacer[10], poljeDugo[q, jt["q", "TAJNO"]]}],
      Row[{poljeDugo[n, jt["n", "JAVNO"]]}],
      Row[{poljeDugo[e, jt["e", "JAVNO"]], Spacer[10], poljeDugo[d, jt["d", "TAJNO"]]}],
      jtLegenda,
      dugme["Provjeri klju\[CHacek]eve", rezKljuc = izlaz[rsaProvjera[n, e, d, p, q]]],
      Dynamic[rezKljuc]}, Spacings -> 1]],
    panel["\[CapitalSHacek]ifriranje i de\[SHacek]ifriranje", Column[{
      Row[{poljeDugo[x, "poruka x"]}],
      dugme["\[CapitalSHacek]ifriraj (y = x^e mod n)",
        (r = izlaz[rsaSif[x, n, e]];
         If[IntegerQ[r],
           y = r;
           rezPoruka = dvaDijela[{izv["RSASifriraj", {x, n, e}, y]},
             Column[{tablica[{"x" -> x, "y = x^e mod n" -> y}], koraciRSASif[x, n, e, y], rsaShema[x, y, None]}, Spacings -> 1]],
           rezPoruka = r])],
      Row[{poljeDugo[y, "\[SHacek]ifrat y"]}],
      dugme["De\[SHacek]ifriraj (x' = y^d mod n)",
        (r = izlaz[rsaDes[y, n, d]];
         If[IntegerQ[r],
           rezPoruka = dvaDijela[{izv["RSADesifriraj", {y, n, d}, r]},
             Column[{tablica[{"x' = x" -> (r == x)}], koraciRSADes[y, n, d, r], rsaShema[x, y, r]}, Spacings -> 1]],
           rezPoruka = r])],
      Dynamic[rezPoruka]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Kartica: Rabin                                                      *)
(* ------------------------------------------------------------------ *)

rabinGen[m_] := If[!IntegerQ[m] || m < 5,
  greska["Bitovna du\[ZHacek]ina m mora biti \[GreaterEqual] 5."],
  With[{r = RabinGenerisiKljuceve[m]}, bbsPromijenjen["RabinGenerisiKljuceve"]; r]
]

rabinProvjera[n_, p_, q_] := If[!cijeliQ[{n, p, q}] || p < 2 || q < 2,
  neCijeli,
  provjeraPrikaz["RabinProvjeriKljuceve", {n, p, q}, RabinProvjeriKljuceve[n, p, q]]
]

rabinSif[x_, n_] := Which[
  !cijeliQ[{x, n}], neCijeli,
  n < 2, greska["Potrebno je n \[GreaterEqual] 2."],
  x < 0 || x >= n, greska["Poruka mora zadovoljavati 0 \[LessEqual] x < n."],
  True, RabinSifriraj[x, n]
]

rabinShema[x_, c_, kand_] := shema[
  "Alice (po\[SHacek]iljalac)", {"javni klju\[CHacek] n", "x = " <> skrati[x], "c = x^2 mod n"},
  "Bob (primalac)", If[kand === None,
    {"tajni klju\[CHacek] (p, q)", "x^2 = c mod n", "\[CHacek]etiri korijena"},
    Prepend[MapIndexed["x_" <> ToString[#2[[1]]] <> " = " <> skrati[#1] &, kand], "tajni klju\[CHacek] (p, q)"]],
  {{"->", "c = " <> skrati[c]}}]

rabinDes[c_, p_, q_, x_] := Which[
  !cijeliQ[{c, p, q, x}], neCijeli,
  p < 3 || q < 3 || Mod[p, 4] != 3 || Mod[q, 4] != 3, greska["Potrebno je p \[Congruent] q \[Congruent] 3 (mod 4)."],
  c < 0 || c >= p*q, greska["\[CapitalSHacek]ifrat mora zadovoljavati 0 \[LessEqual] c < n."],
  True, Module[{k, n = p*q},
    k = RabinDesifriraj[c, p, q];
    dvaDijela[{izv["RabinDesifriraj", {c, p, q}, k]}, Column[{
      tablica[{"Me\:0111u kandidatima je polazni x" -> MemberQ[k, x]}],
      koraciRabinDes[c, p, q, k],
      rabinShema[x, c, k]
    }, Spacings -> 1]]
  ]
]

tabRabin[] := DynamicModule[{m = 16, n = 77, p = 7, q = 11, x = 20, c = 15, r, kl,
  rezKljuc = "", rezPoruka = ""},
  Column[{
    panel["Klju\[CHacek]evi", Column[{
      Row[{polje[m, "bitovna du\[ZHacek]ina prostih brojeva m"], Spacer[15],
        dugme["Generi\[SHacek]i klju\[CHacek]eve",
          (kl = izlaz[rabinGen[m]];
           If[ListQ[kl],
             {n, p, q} = kl;
             rezKljuc = dvaDijela[{izv["RabinGenerisiKljuceve", {m}, kl]},
               Column[{kljucTablica[{{"p", "TAJNO", p}, {"q", "TAJNO", q}, {"n = p\[CenterDot]q", "JAVNO", n}}],
                 binTraka[n, "n = p\[CenterDot]q"], jtLegenda, slucajnaNapomena}, Spacings -> 1]],
             rezKljuc = kl])]}],
      Row[{poljeDugo[p, jt["p", "TAJNO"]], Spacer[10], poljeDugo[q, jt["q", "TAJNO"]]}],
      Row[{poljeDugo[n, jt["n", "JAVNO"]]}],
      jtLegenda,
      dugme["Provjeri klju\[CHacek]eve", rezKljuc = izlaz[rabinProvjera[n, p, q]]],
      Dynamic[rezKljuc]}, Spacings -> 1]],
    panel["\[CapitalSHacek]ifriranje i de\[SHacek]ifriranje", Column[{
      Row[{poljeDugo[x, "poruka x"]}],
      dugme["\[CapitalSHacek]ifriraj (c = x^2 mod n)",
        (r = izlaz[rabinSif[x, n]];
         If[IntegerQ[r],
           c = r;
           rezPoruka = dvaDijela[{izv["RabinSifriraj", {x, n}, c]}, Column[{tablica[{"x" -> x, "c = x^2 mod n" -> c}], rabinShema[x, c, None]}, Spacings -> 1]],
           rezPoruka = r])],
      Row[{poljeDugo[c, "\[SHacek]ifrat c"]}],
      dugme["De\[SHacek]ifriraj (sva \[CHacek]etiri kandidata)", rezPoruka = izlaz[rabinDes[c, p, q, x]]],
      Dynamic[rezPoruka]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Kartica: Diffie-Hellman                                             *)
(* ------------------------------------------------------------------ *)

dhGen[m_] := If[!IntegerQ[m] || m < 5,
  greska["Bitovna du\[ZHacek]ina m mora biti \[GreaterEqual] 5."],
  With[{r = DHGenerisiParametre[m]}, bbsPromijenjen["DHGenerisiParametre"]; r]
]

dhProvjera[p_, g_] := If[!cijeliQ[{p, g}] || p < 3,
  neCijeli,
  provjeraPrikaz["DHProvjeriParametre", {p, g}, DHProvjeriParametre[p, g]]
]

dhSlucajni[p_, g_] := If[!cijeliQ[{p, g}] || p < 5,
  greska["Potreban je prost broj p \[GreaterEqual] 5."],
  (osiguraBBS[];
   {DHGenerisiKljuc[p, g], DHGenerisiKljuc[p, g]})
]

dhRacun[p_, g_, a_, b_] := Which[
  !cijeliQ[{p, g, a, b}], neCijeli,
  p < 3 || a < 1 || b < 1 || a > p - 1 || b > p - 1, greska["Potrebno je p \[GreaterEqual] 3 i 1 \[LessEqual] a, b \[LessEqual] p-1."],
  True, Module[{A, B, KA, KB},
    A = ModPotenciranje[g, a, p];
    B = ModPotenciranje[g, b, p];
    KA = DHZajednickiKljuc[B, a, p];
    KB = DHZajednickiKljuc[A, b, p];
    dvaDijela[{izv["ModPotenciranje", {g, a, p}, A], izv["ModPotenciranje", {g, b, p}, B],
        izv["DHZajednickiKljuc", {B, a, p}, KA], izv["DHZajednickiKljuc", {A, b, p}, KB]},
      Column[{
        tablica[{"Alice i Bob imaju isti klju\[CHacek]" -> (KA == KB)}],
        If[p <= 60 && 1 <= g < p && 1 <= A < p && 1 <= B < p && 1 <= KA < p, dhKruznica[p, g, A, B, KA], Nothing],
        shema[
          "Alice", {"javno: p, g", "tajno: a = " <> skrati[a], "A = " <> skrati[A], "K = " <> skrati[KA]},
          "Bob", {"javno: p, g", "tajno: b = " <> skrati[b], "B = " <> skrati[B], "K = " <> skrati[KB]},
          {{"->", "A = " <> skrati[A]}, {"<-", "B = " <> skrati[B]}}]
      }, Spacings -> 1]]
  ]
]

tabDH[] := DynamicModule[{m = 32, p = 23, g = 5, a = 6, b = 15, kk, kl, rezPar = "", rez = ""},
  Column[{
    panel["Javni parametri", Column[{
      Row[{polje[m, "bitovna du\[ZHacek]ina p"], Spacer[15],
        dugme["Generi\[SHacek]i p i g",
          (kl = izlaz[dhGen[m]];
           If[ListQ[kl],
             {p, g} = kl;
             rezPar = dvaDijela[{izv["DHGenerisiParametre", {m}, kl]},
               Column[{kljucTablica[{{"p", "JAVNO", p}, {"g", "JAVNO", g}}], binTraka[p, "p"], jtLegenda, slucajnaNapomena}, Spacings -> 1]],
             rezPar = kl])]}],
      Row[{poljeDugo[p, jt["p", "JAVNO"]], Spacer[10], poljeDugo[g, jt["g", "JAVNO"]]}],
      jtLegenda,
      dugme["Provjeri parametre", rezPar = izlaz[dhProvjera[p, g]]],
      Dynamic[rezPar]}, Spacings -> 1]],
    panel["Razmjena klju\[CHacek]eva", Column[{
      Row[{poljeDugo[a, jt["a (Alice)", "TAJNO"]], Spacer[10], poljeDugo[b, jt["b (Bob)", "TAJNO"]]}],
      Row[{dugme["Slu\[CHacek]ajni a i b",
          (kk = izlaz[dhSlucajni[p, g]];
           If[ListQ[kk],
             a = kk[[1, 1]]; b = kk[[2, 1]];
             rez = dvaDijela[{izv["DHGenerisiKljuc", {p, g}, kk[[1]]], izv["DHGenerisiKljuc", {p, g}, kk[[2]]]},
               Column[{Style["Tajni brojevi su upisani u polja a i b; zatim pritisnite dugme za zajedni\[CHacek]ki klju\[CHacek].", Gray], slucajnaNapomena}]],
             rez = kk])],
        Spacer[10],
        dugme["Izra\[CHacek]unaj zajedni\[CHacek]ki klju\[CHacek]", rez = izlaz[dhRacun[p, g, a, b]]]}],
      Dynamic[rez]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Kartica: ElGamal                                                    *)
(* ------------------------------------------------------------------ *)

egGen[m_] := If[!IntegerQ[m] || m < 5,
  greska["Bitovna du\[ZHacek]ina m mora biti \[GreaterEqual] 5."],
  With[{r = ElGamalGenerisiKljuceve[m]}, bbsPromijenjen["ElGamalGenerisiKljuceve"]; r]
]

egProvjera[p_, al_, a_, be_] := If[!cijeliQ[{p, al, a, be}] || p < 3,
  neCijeli,
  provjeraPrikaz["ElGamalProvjeriKljuceve", {p, al, a, be}, ElGamalProvjeriKljuceve[p, al, a, be]]
]

egSif[x_, p_, al_, be_] := Which[
  !cijeliQ[{x, p, al, be}], neCijeli,
  p < 5 || x < 1 || x > p - 1, greska["Potrebno je p \[GreaterEqual] 5 i 1 \[LessEqual] x \[LessEqual] p-1."],
  True, (osiguraBBS[]; ElGamalSifriraj[x, p, al, be])
]

egDes[y1_, y2_, p_, a_] := Which[
  !cijeliQ[{y1, y2, p, a}], neCijeli,
  p < 5 || y1 < 1 || y1 > p - 1 || y2 < 0 || y2 > p - 1, greska["Potrebno je 1 \[LessEqual] y1 \[LessEqual] p-1 i 0 \[LessEqual] y2 \[LessEqual] p-1."],
  nzd[ModPotenciranje[y1, a, p], p] != 1, greska["y_1^a nije invertibilan modulo p."],
  True, ElGamalDesifriraj[y1, y2, p, a]
]

egShema[x_, y1_, y2_, xd_] := shema[
  "Alice (po\[SHacek]iljalac)", {"javni klju\[CHacek] (p, \[Alpha], \[Beta])", "x = " <> skrati[x], "k slu\[CHacek]ajan",
    "y_1 = \[Alpha]^k mod p", "y_2 = x\[CenterDot]\[Beta]^k mod p"},
  "Bob (primalac)", {"tajni klju\[CHacek] a", "x = y_2\[CenterDot](y_1^a)^(-1) mod p", If[xd === None, "", "x' = " <> skrati[xd]]},
  {{"->", "y_1 = " <> skrati[y1]}, {"->", "y_2 = " <> skrati[y2]}}]

tabElGamal[] := DynamicModule[{m = 32, p = 2579, al = 2, a = 765, be = 949, x = 1299,
  y1 = 435, y2 = 2396, r, kl, rezKljuc = "", rezPoruka = ""},
  Column[{
    panel["Klju\[CHacek]evi", Column[{
      Row[{polje[m, "bitovna du\[ZHacek]ina p"], Spacer[15],
        dugme["Generi\[SHacek]i klju\[CHacek]eve",
          (kl = izlaz[egGen[m]];
           If[ListQ[kl],
             {p, al, a, be} = kl;
             rezKljuc = dvaDijela[{izv["ElGamalGenerisiKljuceve", {m}, kl]},
               Column[{kljucTablica[{{"p", "JAVNO", p}, {"\[Alpha]", "JAVNO", al}, {"\[Beta] = \[Alpha]^a mod p", "JAVNO", be}, {"a", "TAJNO", a}}],
                 binTraka[p, "p"], jtLegenda, slucajnaNapomena}, Spacings -> 1]],
             rezKljuc = kl])]}],
      Row[{poljeDugo[p, jt["p", "JAVNO"]]}],
      Row[{poljeDugo[al, jt["\[Alpha]", "JAVNO"]], Spacer[10], poljeDugo[be, jt["\[Beta]", "JAVNO"]], Spacer[10], poljeDugo[a, jt["a", "TAJNO"]]}],
      jtLegenda,
      dugme["Provjeri klju\[CHacek]eve", rezKljuc = izlaz[egProvjera[p, al, a, be]]],
      Dynamic[rezKljuc]}, Spacings -> 1]],
    panel["\[CapitalSHacek]ifriranje i de\[SHacek]ifriranje", Column[{
      Row[{poljeDugo[x, "poruka x"]}],
      dugme["\[CapitalSHacek]ifriraj",
        (r = izlaz[egSif[x, p, al, be]];
         If[ListQ[r],
           {y1, y2} = r;
           rezPoruka = dvaDijela[{izv["ElGamalSifriraj", {x, p, al, be}, r]},
             Column[{tablica[{"x" -> x, "y_1" -> y1, "y_2" -> y2}],
               objasni["Jednokratni broj k bira funkcija ElGamalSifriraj preko BBS generatora i ne vra\[CAcute]a ga, pa se tabela \[SHacek]ifriranja ne prikazuje."],
               slucajnaNapomena, egShema[x, y1, y2, None]}, Spacings -> 1]],
           rezPoruka = r])],
      Row[{poljeDugo[y1, "y_1"], Spacer[10], poljeDugo[y2, "y_2"]}],
      dugme["De\[SHacek]ifriraj",
        (r = izlaz[egDes[y1, y2, p, a]];
         If[IntegerQ[r],
           rezPoruka = dvaDijela[{izv["ElGamalDesifriraj", {y1, y2, p, a}, r]},
             Column[{tablica[{"x' = x" -> (r == x)}], koraciEGDes[y1, y2, p, a, r], egShema[x, y1, y2, r]}, Spacings -> 1]],
           rezPoruka = r])],
      Dynamic[rezPoruka]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Kartica: Menezes-Vanstone                                           *)
(* ------------------------------------------------------------------ *)

mvGen[m_] := If[!IntegerQ[m] || m < 6,
  greska["Bitovna du\[ZHacek]ina m mora biti \[GreaterEqual] 6."],
  With[{r = MVGenerisiKljuceve[m]}, bbsPromijenjen["MVGenerisiKljuceve"]; r]
]

mvProvjera[p_, a_, b_, al_, redH_, mT_, be_] := If[!cijeliQ[Join[{p, a, b, redH, mT}, al, be]] || p < 5,
  neCijeli,
  provjeraPrikaz["MVProvjeriKljuceve", {p, a, b, al, redH, mT, be}, MVProvjeriKljuceve[p, a, b, al, redH, mT, be]]
]

mvSlika[a_, b_, p_, al_, redH_, be_, y0_] := If[p <= 3000 && ListQ[al] && ListQ[be],
  Column[{objasni["Narand\[ZHacek]aste ta\[CHacek]ke \[CHacek]ine cikli\[CHacek]nu podgrupu generisanu ta\[CHacek]kom \[Alpha]."],
    grafKrivulje[a, b, p,
      If[redH <= 3000 && ListQ[al], NestList[SabiranjeTacaka[#, al, a, p] &, al, Max[redH - 2, 0]], {}],
      {{Red, "\[Alpha]", al}, {Blue, "\[Beta]", be}, {Darker[Green], Subscript["y", 0], y0}}]}, Spacings -> 0.6],
  Style["(krivulja je prevelika za crtanje)", Gray]
]

mvSif[x1_, x2_, a_, p_, al_, be_, redH_] := Which[
  !cijeliQ[Join[{x1, x2, a, p, redH}, al, be]], neCijeli,
  p < 5 || x1 < 1 || x2 < 1 || x1 > p - 1 || x2 > p - 1, greska["Potrebno je 1 \[LessEqual] x1, x2 \[LessEqual] p-1."],
  redH < 2, greska["redH mora biti \[GreaterEqual] 2."],
  True, (osiguraBBS[]; MVSifriraj[{x1, x2}, a, p, al, be, redH])
]

mvDes[y0_, y1_, y2_, a_, p_, mT_] := Which[
  !cijeliQ[Join[{y1, y2, a, p, mT}, y0]], neCijeli,
  p < 5, greska["Potreban je prost broj p \[GreaterEqual] 5."],
  True, Module[{c},
    c = VisekratnikTacke[mT, y0, a, p];
    If[!ListQ[c] || c[[1]] == 0 || c[[2]] == 0 || nzd[c[[1]], p] != 1 || nzd[c[[2]], p] != 1,
      greska["[m]y_0 ima koordinatu 0 ili je O: de\[SHacek]ifriranje nije mogu\[CAcute]e."],
      MVDesifriraj[{y0, y1, y2}, a, p, mT]
    ]
  ]
]

tabMV[] := DynamicModule[{m = 8, p = 97, a = 2, b = 3, xa = 0, ya = 10, redH = 50, mT = 7, xb = 10, yb = 76,
  x1 = 11, x2 = 22, y0x = 0, y0y = 0, y1 = 0, y2 = 0, r, kl, rezKljuc = "", rezPoruka = ""},
  Column[{
    panel["Klju\[CHacek]evi", Column[{
      Row[{polje[m, "bitovna du\[ZHacek]ina p"], Spacer[15],
        dugme["Generi\[SHacek]i klju\[CHacek]eve",
          (kl = izlaz[mvGen[m]];
           If[ListQ[kl],
             {p, a, b, {xa, ya}, redH, mT, {xb, yb}} = kl;
             rezKljuc = dvaDijela[{izv["MVGenerisiKljuceve", {m}, kl]},
               Column[{kljucTablica[{{"p", "JAVNO", p}, {"a", "JAVNO", a}, {"b", "JAVNO", b}, {"\[Alpha]", "JAVNO", kl[[4]]},
                   {"red od \[Alpha]", "JAVNO", redH}, {"\[Beta] = [m]\[Alpha]", "JAVNO", kl[[7]]}, {"m", "TAJNO", mT}}],
                 mvSlika[a, b, p, {xa, ya}, redH, {xb, yb}, None], jtLegenda, slucajnaNapomena}, Spacings -> 1]],
             rezKljuc = kl])]}],
      Row[{polje[p, jt["p", "JAVNO"], 8], Spacer[8], polje[a, jt["a", "JAVNO"], 6], Spacer[8], polje[b, jt["b", "JAVNO"], 6]}],
      Row[{polje[xa, jt["\[Alpha]_x", "JAVNO"], 6], Spacer[8], polje[ya, jt["\[Alpha]_y", "JAVNO"], 6], Spacer[8], polje[redH, jt["red od \[Alpha]", "JAVNO"], 8]}],
      Row[{polje[mT, jt["m", "TAJNO"], 8], Spacer[8], polje[xb, jt["\[Beta]_x", "JAVNO"], 6], Spacer[8], polje[yb, jt["\[Beta]_y", "JAVNO"], 6]}],
      jtLegenda,
      dugme["Provjeri klju\[CHacek]eve", rezKljuc = izlaz[mvProvjera[p, a, b, {xa, ya}, redH, mT, {xb, yb}]]],
      Dynamic[rezKljuc]}, Spacings -> 1]],
    panel["\[CapitalSHacek]ifriranje i de\[SHacek]ifriranje", Column[{
      Row[{polje[x1, "x_1", 8], Spacer[8], polje[x2, "x_2", 8]}],
      dugme["\[CapitalSHacek]ifriraj",
        (r = izlaz[mvSif[x1, x2, a, p, {xa, ya}, {xb, yb}, redH]];
         If[ListQ[r],
           {{y0x, y0y}, y1, y2} = r;
           rezPoruka = dvaDijela[{izv["MVSifriraj", {{x1, x2}, a, p, {xa, ya}, {xb, yb}, redH}, r]},
             Column[{tablica[{"y_0 = [k]\[Alpha]" -> {y0x, y0y}, "y_1" -> y1, "y_2" -> y2}],
               objasni["Jednokratni broj k bira funkcija MVSifriraj preko BBS generatora i ne vra\[CAcute]a ga, pa se tabela \[SHacek]ifriranja ne prikazuje."],
               slucajnaNapomena,
               mvSlika[a, b, p, {xa, ya}, redH, {xb, yb}, {y0x, y0y}]}, Spacings -> 1]],
           rezPoruka = r])],
      Row[{polje[y0x, "y_{0x}", 6], Spacer[8], polje[y0y, "y_{0y}", 6], Spacer[8],
        polje[y1, "y_1", 8], Spacer[8], polje[y2, "y_2", 8]}],
      dugme["De\[SHacek]ifriraj",
        (r = izlaz[mvDes[{y0x, y0y}, y1, y2, a, p, mT]];
         If[ListQ[r],
           rezPoruka = dvaDijela[{izv["MVDesifriraj", {{{y0x, y0y}, y1, y2}, a, p, mT}, r]},
             Column[{tablica[{"jednako polaznoj poruci" -> (r === {x1, x2})}],
               koraciMVDes[{y0x, y0y}, y1, y2, a, p, mT, r]}, Spacings -> 1]],
           rezPoruka = r])],
      Dynamic[rezPoruka]}, Spacings -> 1]]
  }, Spacings -> 1]
]


(* ------------------------------------------------------------------ *)
(* Glavni prozor                                                       *)
(* ------------------------------------------------------------------ *)

okvir[sadrzaj_] := Pane[sadrzaj, {980, 640}, ImageSizeAction -> "Scrollable", Scrollbars -> Automatic]

KriptografijaGUI[] := CreateDocument[ExpressionCell[
  Column[{
  Row[{Style["Vremensko ograni\[CHacek]enje svakog izra\[CHacek]una [s] (0 = bez ograni\[CHacek]enja): ", Bold],
    InputField[Dynamic[$ogranicenje, Function[val, $ogranicenje = cijeloIzUnosa[val]]], Expression, FieldSize -> 6],
    objasni["   Izra\[CHacek]un se mo\[ZHacek]e prekinuti i s Evaluation > Abort Evaluation."]}],
  TabView[{
    "Uputa" -> okvir[uputa[]],
    "Brojevi" -> okvir[tabBrojevi[]],
    "Prosti brojevi" -> okvir[tabProsti[]],
    "Faktorizacija" -> okvir[tabFakt[]],
    "Elipti\:010dke krivulje" -> okvir[tabEK[]],
    "RSA" -> okvir[tabRSA[]],
    "Rabin" -> okvir[tabRabin[]],
    "Diffie\[Dash]Hellman" -> okvir[tabDH[]],
    "ElGamal" -> okvir[tabElGamal[]],
    "Menezes\[Dash]Vanstone" -> okvir[tabMV[]]
  }]}, Spacings -> 1], "Output"],
  WindowTitle -> "Kriptografija javnog klju\[CHacek]a: grafi\[CHacek]ki interfejs",
  WindowSize -> {1020, 700}
]


End[]

EndPackage[]
