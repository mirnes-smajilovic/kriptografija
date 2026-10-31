(* ::Package:: *)

(* Mathematica Package *)
(* Magistarski rad *)

(* :Title: kriptografija *)
(* :Context: kriptografija` *)
(* :Author: Mirnes Smajilovic *)
(* :Date: 2024-10-01 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 14.1.0.0 *)
(* :Copyright: (c) 2026 Mirnes Smajilovic *)
(* :Keywords: kriptografija*)
(* :Discussion: *)


BeginPackage["Kriptografija`"]


EuklidovAlgoritam::usage = "EuklidovAlgoritam[a, b] ra\[CHacek]una nzd(a,b) i koli\[CHacek]nike q_1,...,q_m Euklidovim algoritmom. Vra\[CAcute]a listu {{q_1,...,q_m}, nzd(a,b)}.";
nzd::usage = "nzd[a, b] nalazi najve\[CAcute]i zajedni\[CHacek]ki djelilac za cijeli broj a i prirodan broj b.";
nzs::usage = "nzs[a,b] ra\[CHacek]una najmanji zajedni\[CHacek]ki sadr\[ZHacek]ilac brojeva a i b: d=nzd(a,b), z=(a/d)*b.";
ProsireniEuklid::usage = "ProsireniEuklid[a, b] za cijeli broj a i prirodan broj b vra\[CAcute]a listu {nzd(a,b), x, y} takvu da je a*x+b*y=nzd(a,b).";
MultiplikativniInverz::usage = "MultiplikativniInverz[a, n] ra\[CHacek]una multiplikativni inverz broja a modulo n, ako postoji, kao poseban slu\[CHacek]aj pro\[SHacek]irenog Euklidovog algoritma.";
KineskiTeoremOOstacima::usage = "KineskiTeoremOOstacima[a, m] rje\[SHacek]ava sistem kongruencija x\[Congruent]a[[j]] (mod m[[j]]), za module u parovima relativno proste. Koristi funkciju MultiplikativniInverz. Vra\[CAcute]a jedinstveno rje\[SHacek]enje x modulo M=\[Product]m[[j]].";
ModPotenciranje::usage = "ModPotenciranje[a, n, m] ra\[CHacek]una a^n mod m algoritmom kvadriraj i mno\[ZHacek]i.";
RedElementa::usage = "RedElementa[a, n, faktori] ra\[CHacek]una red elementa a modulo n, pri \[CHacek]emu je faktori faktorizacija broja phi(n) u obliku liste parova {{p1,e1},{p2,e2},...}.";
PrimitivniKorijen::usage = "PrimitivniKorijen[n, faktori] vra\[CAcute]a slu\[CHacek]ajno odabran primitivni korijen modulo n, pri \[CHacek]emu je faktori faktorizacija broja phi(n) u obliku liste parova {{p1,e1},...}.";
PrimitivniKorijenNajmanji::usage = "PrimitivniKorijenNajmanji[n, faktori] vra\[CAcute]a najmanji primitivni korijen modulo n, pri \[CHacek]emu je faktori faktorizacija broja phi(n) u obliku liste parova {{p1,e1},...}.";
JacobijevSimbol::usage = "JacobijevSimbol[a, m] ra\[CHacek]una Jacobijev (Legendreov) simbol (a/m), za neparan prirodan broj m>=3 i proizvoljan cijeli broj a.";
ParcijalniKvocijenti::usage = "ParcijalniKvocijenti[a, b] ra\[CHacek]una parcijalne kvocijente razvoja racionalnog broja a/b u veri\[ZHacek]ni razlomak, koriste\[CAcute]i Euklidov algoritam.";
Konvergente::usage = "Konvergente[kolicnici] ra\[CHacek]una konvergente veri\[ZHacek]nog razlomka na osnovu liste parcijalnih kvocijenata. Vra\[CAcute]a listu parova {p_i,q_i}.";
FermatovTest::usage = "FermatovTest[n, k] provodi Fermatov vjerovatnosni test prostosti nad brojem n, uz k ponavljanja. Vra\[CAcute]a True ako je n vjerovatno prost, False ako je sigurno slo\[ZHacek]en.";
SolovayStrassenTest::usage = "SolovayStrassenTest[n, k] provodi Solovay-Strassenov vjerovatnosni test prostosti nad brojem n, uz k ponavljanja. Vra\[CAcute]a True ako je n vjerovatno prost, False ako je sigurno slo\[ZHacek]en.";
PotrebnoIteracijaFSS::usage = "PotrebnoIteracijaFSS[p] ra\[CHacek]una minimalan broj iteracija k potreban da Fermatov ili Solovay-Strassenov test tvrdi da je n prost s vjerovatno\[CAcute]om ve\[CAcute]om od p.";
MillerRabinTest::usage = "MillerRabinTest[n, k] provodi Miller-Rabinov vjerovatnosni test prostosti nad brojem n, uz k ponavljanja. Vra\[CAcute]a True ako je n vjerovatno prost, False ako je sigurno slo\[ZHacek]en.";
PotrebnoIteracijaMillerRabin::usage = "PotrebnoIteracijaMillerRabin[p] ra\[CHacek]una minimalan broj iteracija k potreban da Miller-Rabinov test tvrdi da je n prost s vjerovatno\[CAcute]om ve\[CAcute]om od p.";
BBSInicijalizacija::usage = "BBSInicijalizacija[m, k] generi\[SHacek]e dva m-bitna prosta broja p i q, p==q==3 (mod 4), izra\[CHacek]unava modul n=pq i po\[CHacek]etno stanje s0. Argument k (podrazumijevano 20) odre\:0111uje broj iteracija Miller-Rabinovog testa kori\[SHacek]tenog za provjeru prostosti. Vra\[CAcute]a {n, s0}.";
BBSPokreni::usage = "BBSPokreni[m, k] inicijalizira BBS generator za m-bitne proste faktore i postavlja interno stanje za budu\[CAcute]e pozive. Argument k (podrazumijevano 20) odre\:0111uje broj iteracija Miller-Rabinovog testa.";
BBSGenerisiBitove::usage = "BBSGenerisiBitove[n, s, l] je pomo\[CAcute]na funkcija koja generi\[SHacek]e niz od l pseudoslu\[CHacek]ajnih bitova BBS generatorom, za modul n i posljednje poznato stanje s. Vra\[CAcute]a {bitovi, novo_stanje}. Ne poziva se direktno; koristi je funkcija BBSSljedeciBitovi.";
BBSSljedeciBitovi::usage = "BBSSljedeciBitovi[l] vra\[CAcute]a sljede\[CAcute]ih l bitova iz ve\[CAcute] pokrenutog BBS generatora, a\[ZHacek]uriraju\[CAcute]i interno stanje.";
BBSBrojIzOpsega::usage = "BBSBrojIzOpsega[a, b] vra\[CAcute]a slu\[CHacek]ajan cijeli broj u opsegu [a,b], generisan pokrenutim BBS generatorom, koriste\[CAcute]i metodu odbacivanja radi uniformne raspodjele.";
BBSBroj::usage = "BBSBroj[k] vra\[CAcute]a slu\[CHacek]ajan cijeli broj od ta\[CHacek]no k bitova (vode\[CAcute]i bit uvijek 1), generisan pokrenutim BBS generatorom.";
EratostenovoSito::usage = "EratostenovoSito[n] vra\[CAcute]a listu svih prostih brojeva manjih ili jednakih n, koriste\[CAcute]i Eratostenovo sito.";
SlucajanProst::usage = "SlucajanProst[m, k, B] generi\[SHacek]e slu\[CHacek]ajan m-bitni vjerovatno prost broj. Argument k (podrazumijevano 20) odre\:0111uje broj iteracija Miller-Rabinovog testa, a B (podrazumijevano 10000) granicu probnog dijeljenja. Prije poziva ove funkcije potrebno je pokrenuti generator pozivom funkcije BBSPokreni.";
SljedeciProst::usage = "SljedeciProst[n, k, B] pronalazi najmanji prost broj strogo ve\[CAcute]i od n. Argument k (podrazumijevano 20) odre\:0111uje broj iteracija Miller-Rabinovog testa, a B (podrazumijevano 10000) granicu probnog dijeljenja.";
SlucajanProstBrojPokusaja::usage = "SlucajanProstBrojPokusaja[m, k, B] instrumentirana je verzija funkcije SlucajanProst, isklju\[CHacek]ivo za potrebe eksperimentalne analize. Prati potpuno isti algoritam (probno dijeljenje prostim brojevima do B, zatim MillerRabinTest), ali dodatno vra\[CAcute]a broj ukupno generisanih kandidata i broj kandidata koji su nakon probnog dijeljenja poslani na MillerRabinTest. Vra\[CAcute]a {kandidat, ukupnoKandidata, poslanoNaMR}.";
FaktorizacijaDijeljenjem::usage = "FaktorizacijaDijeljenjem[N] vra\[CAcute]a listu parova {p,e} takvih da je N=p1^e1*p2^e2*...*ps^es, dobijenu probnim dijeljenjem nizom djelilaca 2,3,5,7,9,11,...";
FermatovaFaktorizacija::usage = "FermatovaFaktorizacija[n] za neparan slo\[ZHacek]en broj n koji nije potpun kvadrat vra\[CAcute]a par {p,q} netrivijalnih faktora takvih da je n=p*q i 1<q<p<n, koriste\[CAcute]i Fermatovu metodu faktorizacije. Vra\[CAcute]a poruku ako netrivijalni faktori nisu prona\:0111eni.";
PollardovaPMinus1::usage = "PollardovaPMinus1[n, Bmin, Bmax, a] tra\[ZHacek]i netrivijalni djelilac slo\[ZHacek]enog broja n Pollardovom p-1 metodom, pove\[CAcute]avaju\[CAcute]i granicu glatko\[CAcute]e B od Bmin do Bmax uz fiksno a (podrazumijevano a=2). Vra\[CAcute]a djelilac d, 1<d<n, ili poruku o neuspjehu.";
PollardovaPMinus1Prosireno::usage = "PollardovaPMinus1Prosireno[n, Bmin, Bmax, aMin, aMax] tra\[ZHacek]i netrivijalni djelilac slo\[ZHacek]enog broja n Pollardovom p-1 metodom, kombinovanim pomjeranjem granice glatko\[CAcute]e B (od Bmin do Bmax) i a (od aMin do aMax, podrazumijevano 2 do 5). Za svaki B se prvo isprobaju sve vrijednosti a prije prelaska na sljede\[CAcute]i B. Vra\[CAcute]a djelilac d, 1<d<n, ili poruku o neuspjehu.";
PollardovaRho::usage = "PollardovaRho[n, x0, c] pronalazi netrivijalni djelilac slo\[ZHacek]enog broja n osnovnom Pollardovom rho metodom, koriste\[CAcute]i preslikavanje f(x)=(x^2+c) mod n i po\[CHacek]etnu vrijednost x0 (podrazumijevano x0=2, c=1). Vra\[CAcute]a djelilac p, 1<p<n, ili poruku o neuspjehu.";
PollardovaRhoFloyd::usage = "PollardovaRhoFloyd[n, x0, c] pronalazi netrivijalni djelilac slo\[ZHacek]enog broja n Pollardovom rho metodom uz Floydovo pobolj\[SHacek]anje, koriste\[CAcute]i preslikavanje f(x)=(x^2+c) mod n. Vra\[CAcute]a djelilac p, 1<p<n, ili poruku o neuspjehu.";
PollardovaRhoBrent::usage = "PollardovaRhoBrent[n, x0, c, m] pronalazi netrivijalni djelilac slo\[ZHacek]enog broja n Brentovom verzijom Pollardove rho metode, koja koristi blokovsko akumuliranje razlika radi rje\:0111eg ra\[CHacek]unanja nzd. Parametar m (podrazumijevano 32) odre\:0111uje veli\[CHacek]inu unutra\[SHacek]njeg bloka. Vra\[CAcute]a djelilac G, 1<G<n, ili poruku o neuspjehu.";
RastaviNaProsteFaktore::usage = "RastaviNaProsteFaktore[n] pomo\[CAcute]na je funkcija koja vra\[CAcute]a listu svih prostih faktora broja n (sa ponavljanjem), koriste\[CAcute]i probno dijeljenje malim prostim brojevima (do 10000) i PollardovaRho za ve\[CAcute]e faktore, rekurzivno primijenjeno dok svi dobijeni faktori nisu potvr\:0111eni prosti pomo\[CAcute]u MillerRabinTest.";
PotpunaFaktorizacija::usage = "PotpunaFaktorizacija[n] vra\[CAcute]a faktorizaciju broja n u obliku {{p1,e1},{p2,e2},...}, koriste\[CAcute]i RastaviNaProsteFaktore.";
EulerovaFunkcija::usage = "EulerovaFunkcija[n] ra\[CHacek]una Eulerovu funkciju \[Phi](n).";
MetodaFaktorskeBaze::usage = "MetodaFaktorskeBaze[n, w, maksPokusaja] poku\[SHacek]ava prona\[CAcute]i netrivijalni djelilac neparnog slo\[ZHacek]enog broja n metodom faktorske baze, sa granicom glatko\[CAcute]e w koja odre\:0111uje bazu B={-1}\[Union]{prosti brojevi \[LessEqual] w}. Slu\[CHacek]ajno bira cijele brojeve b, provjerava mo\[ZHacek]e li se apsolutno najmanji ostatak od b^2 mod n rastaviti po bazi B, i nakon prikupljanja dovoljnog broja relacija tra\[ZHacek]i netrivijalnu linearnu zavisnost nad Z_2. Argument maksPokusaja (podrazumijevano 20000) odre\:0111uje najve\[CAcute]i broj poku\[SHacek]aja, odnosno slu\[CHacek]ajno odabranih brojeva b, prije prekida pretrage. Vra\[CAcute]a netrivijalni djelilac ili poruku o neuspjehu.";
DixonovAlgoritam::usage = "DixonovAlgoritam[n, Bbound, maksPokusaja] predstavlja konkretnu realizaciju metode faktorske baze: slu\[CHacek]ajno bira brojeve r, provjerava je li r^2 mod n B-gladak broj (B={prosti brojevi \[LessEqual] Bbound}), i nakon prikupljanja dovoljnog broja relacija tra\[ZHacek]i netrivijalnu linearnu zavisnost nad Z_2 kako bi konstruisao x i y sa x^2\[Congruent]y^2 (mod n). Argument maksPokusaja (podrazumijevano 20000) odre\:0111uje najve\[CAcute]i broj poku\[SHacek]aja, odnosno slu\[CHacek]ajno odabranih brojeva r, prije prekida pretrage. Vra\[CAcute]a netrivijalni djelilac ili poruku o neuspjehu.";
ValidnaKrivulja::usage = "ValidnaKrivulja[a, b, p] provjerava da li elipti\[CHacek]ka krivulja y^2=x^3+ax+b nad F_p zadovoljava uslov nesingularnosti 4a^3+27b^2 \[NotEqual] 0 (mod p). Vra\[CAcute]a True ili False.";
TackaNaKrivulji::usage = "TackaNaKrivulji[x, y, a, b, p] provjerava da li ta\[CHacek]ka (x,y) le\[ZHacek]i na elipti\[CHacek]koj krivulji y^2=x^3+ax+b nad F_p, tj. da li vrijedi y^2 \[Congruent] x^3+ax+b (mod p). Vra\[CAcute]a True ili False.";
SabiranjeTacaka::usage = "SabiranjeTacaka[P, Q, a, p] sabira dvije ta\[CHacek]ke P i Q na elipti\[CHacek]koj krivulji y^2=x^3+ax+b nad F_p, prema formulama za \[Lambda], x3 i y3. Ta\[CHacek]ka u beskona\[CHacek]nosti se predstavlja simbolom O (veliko slovo O). Pokriva slu\[CHacek]ajeve P=O, Q=O, Q=-P i P=Q (dupliranje). Koristi funkciju MultiplikativniInverz.";
VisekratnikTacke::usage = "VisekratnikTacke[k, P, a, p] ra\[CHacek]una vi\[SHacek]ekratnik [k]P ta\[CHacek]ke P na elipti\[CHacek]koj krivulji nad F_p, algoritmom 'udvostru\[CHacek]i i saberi' (binarni zapis broja k, obradom bitova od najzna\[CHacek]ajnijeg ka najmanje zna\[CHacek]ajnom). Koristi funkciju SabiranjeTacaka.";
RedTacke::usage = "RedTacke[P, a, p] ra\[CHacek]una red ta\[CHacek]ke |P| na elipti\[CHacek]koj krivulji nad F_p, tj. najmanji prirodan broj k takav da je [k]P=O, ponavljanim sabiranjem ta\[CHacek]ke P sa samom sobom. Koristi funkciju SabiranjeTacaka.";
RedKrivuljeBruteForce::usage = "RedKrivuljeBruteForce[a, b, p] ra\[CHacek]una red elipti\[CHacek]ke krivulje #E(F_p), y^2=x^3+ax+b, direktno preko formule #E(F_p)=1+p+\[Sum]JacobijevSimbol[x^3+ax+b,p], sumiraju\[CAcute]i po svim x\[Element]F_p. Efikasno samo za manje vrijednosti p (do otprilike 10^4).";
RedKrivuljeShanksMestre::usage = "RedKrivuljeShanksMestre[a, b, p, P] ra\[CHacek]una red elipti\[CHacek]ke krivulje #E(F_p) Shanks-Mestreovom metodom (baby-step/giant-step) za zadanu ta\[CHacek]ku P. Ako je kandidat jedinstven, vra\[CAcute]a #E(F_p); ina\[CHacek]e vra\[CAcute]a listu svih prona\:0111enih kandidata uz poruku da treba probati novu ta\[CHacek]ku P.";
RedKrivulje::usage = "RedKrivulje[a, b, p, P] ra\[CHacek]una red elipti\[CHacek]ke krivulje #E(F_p), y^2=x^3+ax+b. Za p\[LessEqual]10^4 automatski koristi RedKrivuljeBruteForce (P se ne koristi). Za p>10^4 koristi RedKrivuljeShanksMestre, za \[SHacek]ta je obavezan i \[CHacek]etvrti parametar P (ta\[CHacek]ka na krivulji).";
GeneratoriKrivulje::usage = "GeneratoriKrivulje[a, b, p] pronalazi sve generatore cikli\[CHacek]ne grupe E(F_p) elipti\[CHacek]ke krivulje y^2=x^3+ax+b, tj. sve ta\[CHacek]ke \[CHacek]iji je red jednak redu cijele grupe #E(F_p). Prazna lista zna\[CHacek]i da grupa nije cikli\[CHacek]ka. Koristi RedKrivuljeBruteForce i RedTacke.";
HasseProvjera::usage = "HasseProvjera[p, red] provjerava da li vrijednost red pripada intervalu [p+1-2\[Sqrt]p, p+1+2\[Sqrt]p] dozvoljenom Hasseovim teoremom za red elipti\[CHacek]ke krivulje nad F_p. Vra\[CAcute]a True ili False.";
RSAGenerisiKljuceve::usage = "RSAGenerisiKljuceve[m, k] generi\[SHacek]e RSA klju\[CHacek]eve: dva razli\[CHacek]ita m-bitna prosta broja p i q (koriste\[CAcute]i SlucajanProst), n=pq, \[Phi](n)=(p-1)(q-1), i eksponent e slu\[CHacek]ajno biran iz opsega (10^5,\[Phi](n)-1) preko BBSBrojIzOpsega dok se ne prona\:0111e e relativno prost sa \[Phi](n). Tajni eksponent d ra\[CHacek]una se preko MultiplikativniInverz. Vra\[CAcute]a {n, e, d, p, q}.";
RSAProvjeriKljuceve::usage = "RSAProvjeriKljuceve[n, e, d, p, q, k] provjerava validnost RSA parametara: da li je n=pq, da li su p i q (vjerovatno) prosti (MillerRabinTest sa k iteracija, podrazumijevano 20), da li je nzd(e,\[Phi](n))=1, i da li je d\[CenterDot]e\[Congruent]1 (mod \[Phi](n)). Vra\[CAcute]a True ako su svi uslovi zadovoljeni, ili listu tekstualnih opisa svih uo\[CHacek]enih problema.";
RSASifriraj::usage = "RSASifriraj[x, n, e] \[SHacek]ifrira poruku x RSA kriptosistemom: y=x^e mod n.";
RSADesifriraj::usage = "RSADesifriraj[y, n, d] de\[SHacek]ifrira \[SHacek]ifrat y RSA kriptosistemom: x=y^d mod n.";
RabinGenerisiKljuceve::usage = "RabinGenerisiKljuceve[m, k] generi\[SHacek]e Rabinove klju\[CHacek]eve: dva razli\[CHacek]ita m-bitna prosta broja p i q, oba kongruentna 3 (mod 4) (koriste\[CAcute]i SlucajanProst). Vra\[CAcute]a {n, p, q}.";
RabinProvjeriKljuceve::usage = "RabinProvjeriKljuceve[n, p, q, k] provjerava validnost Rabinovih parametara: da li je n=pq, da li su p i q (vjerovatno) prosti (MillerRabinTest sa k iteracija), i da li je p\[Congruent]q\[Congruent]3 (mod 4). Vra\[CAcute]a True ili listu tekstualnih opisa uo\[CHacek]enih problema.";
RabinSifriraj::usage = "RabinSifriraj[x, n] \[SHacek]ifrira otvoreni tekst x Rabinovim kriptosistemom: c=x^2 mod n.";
RabinDesifriraj::usage = "RabinDesifriraj[c, p, q] de\[SHacek]ifrira \[SHacek]ifrat c Rabinovim kriptosistemom, koriste\[CAcute]i ProsireniEuklid i Kineski teorem o ostacima. Vra\[CAcute]a listu sva \[CHacek]etiri mogu\[CAcute]a kandidata za otvoreni tekst.";
DHGenerisiParametre::usage = "DHGenerisiParametre[m, k] generi\[SHacek]e javne parametre Diffie-Hellmanovog protokola: prost broj p (m-bitni, preko SlucajanProst) i generator g grupe Z_p^*, dobijen faktorizacijom p-1 preko PotpunaFaktorizacija i pozivom PrimitivniKorijen. Vra\[CAcute]a {p, g}.";
DHProvjeriParametre::usage = "DHProvjeriParametre[p, g, k] provjerava validnost DH parametara: da li je p prost (MillerRabinTest sa k iteracija, podrazumijevano 20), i da li je g zaista generator (primitivni korijen) grupe Z_p^*, tj. da li je red od g jednak p-1 (preko PotpunaFaktorizacija i RedElementa). Vra\[CAcute]a True ako su svi uslovi zadovoljeni, ili listu tekstualnih opisa svih uo\[CHacek]enih problema.";
DHGenerisiKljuc::usage = "DHGenerisiKljuc[p, g] generi\[SHacek]e tajni broj a\[Element]{1,...,p-1} (preko BBSBrojIzOpsega) i pripadnu javnu vrijednost A=g^a mod p. Vra\[CAcute]a {a, A}. Koristi je svaka strana samostalno (Alice i Bob).";
DHZajednickiKljuc::usage = "DHZajednickiKljuc[strana, tajni, p] ra\[CHacek]una zajedni\[CHacek]ki tajni klju\[CHacek] K=strana^tajni mod p. Alice poziva funkciju s Bobovom javnom vrijedno\[SHacek]\[CAcute]u B i svojim tajnim brojem a, dok Bob poziva funkciju s javnom vrijedno\[SHacek]\[CAcute]u A koju je poslala Alice i svojim tajnim brojem b. Oboje dobijaju isti klju\[CHacek] K.";
ElGamalGenerisiKljuceve::usage = "ElGamalGenerisiKljuceve[m, k] generi\[SHacek]e ElGamal klju\[CHacek]eve: prost broj p (m-bitni, preko SlucajanProst), primitivni korijen alpha (preko PotpunaFaktorizacija i PrimitivniKorijen), tajni broj a (preko BBSBrojIzOpsega), i beta=alpha^a mod p. Vra\[CAcute]a {p, alpha, a, beta}.";
ElGamalProvjeriKljuceve::usage = "ElGamalProvjeriKljuceve[p, alpha, a, beta, k] provjerava validnost ElGamal parametara: da li je p prost, da li je alpha primitivni korijen (preko PotpunaFaktorizacija i RedElementa), i da li je beta=alpha^a mod p. Vra\[CAcute]a True ako su svi uslovi zadovoljeni, ili listu tekstualnih opisa svih uo\[CHacek]enih problema.";
ElGamalSifriraj::usage = "ElGamalSifriraj[x, p, alpha, beta] \[SHacek]ifrira poruku x ElGamalovim kriptosistemom: bira slu\[CHacek]ajan jednokratni k preko BBSBrojIzOpsega, ra\[CHacek]una y1=alpha^k mod p i y2=x*beta^k mod p. Vra\[CAcute]a {y1, y2}.";
ElGamalDesifriraj::usage = "ElGamalDesifriraj[y1, y2, p, a] de\[SHacek]ifrira \[SHacek]ifrat (y1,y2) ElGamalovim kriptosistemom: x=y2*(y1^a)^(-1) mod p, koriste\[CAcute]i ModPotenciranje i MultiplikativniInverz.";
MVGenerisiKljuceve::usage = "MVGenerisiKljuceve[m, k] generi\[SHacek]e Menezes-Vanstoneove klju\[CHacek]eve: prost broj p (preko SlucajanProst), validnu elipti\[CHacek]ku krivulju (a,b) nad F_p (preko ValidnaKrivulja), ta\[CHacek]ku alpha na krivulji (prona\:0111enu direktnom pretragom uz pomo\[CAcute] JacobijevSimbol), red podgrupe generisane sa alpha (preko RedTacke), tajni broj m (preko BBSBrojIzOpsega), i beta=[m]alpha (preko VisekratnikTacke). Vra\[CAcute]a {p, a, b, alpha, redH, m, beta}.";
MVProvjeriKljuceve::usage = "MVProvjeriKljuceve[p, a, b, alpha, redH, m, beta] provjerava validnost MV parametara: da li je krivulja (a,b) nad F_p validna, da li je alpha na krivulji, da li je redH stvarni red od alpha (preko RedTacke), i da li je beta=[m]alpha. Vra\[CAcute]a True ako su svi uslovi zadovoljeni, ili listu tekstualnih opisa svih uo\[CHacek]enih problema.";
MVSifriraj::usage = "MVSifriraj[{x1,x2}, a, p, alpha, beta, redH] \[SHacek]ifrira poruku (x1,x2)\[Element]Z_p^*\[Times]Z_p^* Menezes-Vanstoneovim kriptosistemom: bira slu\[CHacek]ajan jednokratni k preko BBSBrojIzOpsega (izbjegavaju\[CAcute]i degenerisan slu\[CHacek]aj c1=0 ili c2=0), ra\[CHacek]una y0=[k]alpha i (c1,c2)=[k]beta, te y1=c1*x1 mod p i y2=c2*x2 mod p. Vra\[CAcute]a {y0,y1,y2}.";
MVDesifriraj::usage = "MVDesifriraj[{y0,y1,y2}, a, p, m] de\[SHacek]ifrira \[SHacek]ifrat Menezes-Vanstoneovim kriptosistemom: ra\[CHacek]una (c1,c2)=[m]y0, zatim x1=y1*c1^(-1) mod p i x2=y2*c2^(-1) mod p. Vra\[CAcute]a {x1,x2}.";


Begin["`Private`"]


EuklidovAlgoritam[a_Integer, b_Integer] := Module[
  {r, q, m},
  r[0] = a;
  r[1] = b;
  m = 1;
  While[r[m] != 0,
    q[m] = Floor[r[m - 1]/r[m]];
    r[m + 1] = r[m - 1] - q[m]*r[m];
    m++
  ];
  m--;
  {Table[q[i], {i, 1, m}], r[m]}
]

nzd[a_Integer, b_Integer] := Module[
  {r0 = a, r1 = b, r2},
  While[r1 != 0,
    r2 = Mod[r0, r1];
    r0 = r1;
    r1 = r2
  ];
  r0
]

ProsireniEuklid[a_Integer, b_Integer] := Module[
  {r, x, y, q, i, k},
  r[-1] = a; r[0] = b;
  x[-1] = 1; x[0] = 0;
  y[-1] = 0; y[0] = 1;
  i = 1;
  While[r[i - 1] > 0,
    q[i] = Floor[r[i - 2]/r[i - 1]];
    r[i] = r[i - 2] - q[i]*r[i - 1];
    x[i] = x[i - 2] - q[i]*x[i - 1];
    y[i] = y[i - 2] - q[i]*y[i - 1];
    i++
  ];
  k = i - 2;
  {r[k], x[k], y[k]}
]

nzs[a_Integer, b_Integer] := Module[
  {d, z},
  d = nzd[a, b];
  z = (a/d)*b;
  z
]

MultiplikativniInverz[a_Integer, n_Integer] := Module[
  {r, y, q, i, k, rezultat},
  r[-1] = n; r[0] = a;
  y[-1] = 0; y[0] = 1;
  i = 1;
  While[r[i - 1] > 0,
    q[i] = Floor[r[i - 2]/r[i - 1]];
    r[i] = r[i - 2] - q[i]*r[i - 1];
    y[i] = y[i - 2] - q[i]*y[i - 1];
    i++
  ];
  k = i - 2;
  If[r[k] != 1,
    Print["Multiplikativni inverz ne postoji."], Mod[y[k], n]
  ]
]

KineskiTeoremOOstacima[a_List, m_List] := Module[
  {M, x, r, j, n, u},
  r = Length[m];
  M = 1;
  For[j = 1, j <= r, j++, M = M*m[[j]]];
  x = 0;
  For[j = 1, j <= r, j++,
    n = M/m[[j]];
    u = MultiplikativniInverz[Mod[n, m[[j]]], m[[j]]];
    x = x + a[[j]]*n*u
  ];
  x = Mod[x, M];
  x
]

ModPotenciranje[a_Integer, n_Integer, m_Integer] := Module[
  {bitovi, s, y},
  bitovi = IntegerDigits[n, 2];
  s = Length[bitovi];
  y = 1;
  Do[
    y = Mod[y^2, m];
    If[bitovi[[k]] == 1,
      y = Mod[y*a, m]
    ],
    {k, 1, s}
  ];
  y
]

RedElementa[a_Integer, n_Integer, faktori_List] := Module[
  {d, b, p, e, par},
  d = Product[par[[1]]^par[[2]], {par, faktori}];
  Do[
    p = par[[1]];
    e = par[[2]];
    d = d/p^e;
    b = ModPotenciranje[a, d, n];
    While[b != 1,
      b = ModPotenciranje[b, p, n];
      d = d*p
    ],
    {par, faktori}
  ];
  d
]

PrimitivniKorijen[n_Integer, faktori_List] := Module[
  {phi, prosti, g, primitivan, p, par},
  phi = Product[par[[1]]^par[[2]], {par, faktori}];
  prosti = faktori[[All, 1]];
  SeedRandom[Method -> "OpenSSL"];
  While[True,
    g = RandomInteger[{1, n - 1}];
    If[nzd[g, n] == 1,
      primitivan = True;
      Do[
        If[ModPotenciranje[g, phi/p, n] == 1,
          primitivan = False;
          Break[]
        ],
        {p, prosti}
      ];
      If[primitivan, Return[g]]
    ]
  ]
]

PrimitivniKorijenNajmanji[n_Integer, faktori_List] := Module[
  {phi, prosti, g, primitivan, p, par},
  phi = Product[par[[1]]^par[[2]], {par, faktori}];
  prosti = faktori[[All, 1]];
  For[g = 1, g <= n - 1, g++,
    If[nzd[g, n] == 1,
      primitivan = True;
      Do[
        If[ModPotenciranje[g, phi/p, n] == 1,
          primitivan = False;
          Break[]
        ],
        {p, prosti}
      ];
      If[primitivan, Return[g]]
    ]
  ]
]

JacobijevSimbol[a_Integer, m_Integer] := Module[
  {aa, mm, t},
  aa = Mod[a, m];
  mm = m;
  t = 1;
  While[aa != 0,
    While[Mod[aa, 2] == 0,
      aa = aa/2;
      If[Mod[mm, 8] == 3 || Mod[mm, 8] == 5, t = -t]
    ];
    {aa, mm} = {mm, aa};
    If[Mod[aa, 4] == 3 && Mod[mm, 4] == 3, t = -t];
    aa = Mod[aa, mm]
  ];
  If[mm == 1, t, 0]
]

ParcijalniKvocijenti[a_Integer, b_Integer] := First[EuklidovAlgoritam[a, b]]

Konvergente[kolicnici_List] := Module[
  {p, q, n, i},
  n = Length[kolicnici];
  p[-2] = 0; p[-1] = 1;
  q[-2] = 1; q[-1] = 0;
  Do[
    p[i] = kolicnici[[i + 1]]*p[i - 1] + p[i - 2];
    q[i] = kolicnici[[i + 1]]*q[i - 1] + q[i - 2],
    {i, 0, n - 1}
  ];
  Table[{p[i], q[i]}, {i, 0, n - 1}]
]

FermatovTest[n_Integer, k_Integer] := Module[
  {b, r, i},
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    r = ModPotenciranje[b, n - 1, n];
    If[r != 1, Return[False]];
    i++
  ];
  True
]

SolovayStrassenTest[n_Integer, k_Integer] := Module[
  {b, r, s, i},
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    r = ModPotenciranje[b, (n - 1)/2, n];
    If[r != 1 && r != n - 1, Return[False]];
    s = Mod[JacobijevSimbol[b, n], n];
    If[r != s, Return[False]];
    i++
  ];
  True
]

MillerRabinTest[n_Integer, k_Integer] := Module[
  {s, t, i, b, y, j},
  s = 0;
  t = n - 1;
  While[Mod[t, 2] == 0,
    t = t/2;
    s++
  ];
  i = 1;
  SeedRandom[Method -> "OpenSSL"];
  While[i <= k,
    b = RandomInteger[{2, n - 2}];
    y = ModPotenciranje[b, t, n];
    If[y != 1 && y != n - 1,
      j = 1;
      While[j < s && y != n - 1,
        y = ModPotenciranje[y, 2, n];
        If[y == 1, Return[False]];
        j++
      ];
      If[y != n - 1, Return[False]]
    ];
    i++
  ];
  True
]

PotrebnoIteracijaMillerRabin[p_] := Ceiling[Log[2, 1/(1 - p)]/2]

PotrebnoIteracijaFSS[p_] := Ceiling[Log[2, 1/(1 - p)]]

BBSInicijalizacija[m_Integer, k_Integer : 20] := Module[
  {p, q, n, s, s0, kandidat},
  p = 0;
  SeedRandom[Method -> "OpenSSL"];
  While[p == 0,
    kandidat = RandomInteger[{2^(m - 1), 2^m - 1}];
    If[EvenQ[kandidat], kandidat = kandidat + 1];
    If[Mod[kandidat, 4] == 3 && MillerRabinTest[kandidat, k],
      p = kandidat
    ]
  ];
  q = 0;
  While[q == 0,
    kandidat = RandomInteger[{2^(m - 1), 2^m - 1}];
    If[EvenQ[kandidat], kandidat = kandidat + 1];
    If[kandidat != p && Mod[kandidat, 4] == 3 && MillerRabinTest[kandidat, k],
      q = kandidat
    ]
  ];
  n = p*q;
  s = 0;
  While[s == 0,
    kandidat = RandomInteger[{1, n - 1}];
    If[nzd[kandidat, n] == 1,
      s = kandidat
    ]
  ];
  s0 = Mod[s^2, n];
  {n, s0}
]

BBSPokreni[m_Integer, k_Integer : 20] := Module[{rezultat},
  rezultat = BBSInicijalizacija[m, k];
  BBSModul = rezultat[[1]];
  BBSStanje = rezultat[[2]];
]

BBSGenerisiBitove[n_Integer, s_Integer, l_Integer] := Module[
  {trenutno, bitovi},
  trenutno = s;
  bitovi = {};
  Do[
    trenutno = Mod[trenutno^2, n];
    AppendTo[bitovi, Mod[trenutno, 2]],
    {l}
  ];
  {bitovi, trenutno}
]

BBSSljedeciBitovi[l_Integer] := Module[{rezultat},
  rezultat = BBSGenerisiBitove[BBSModul, BBSStanje, l];
  BBSStanje = rezultat[[2]];
  rezultat[[1]]
]

BBSBrojIzOpsega[a_Integer, b_Integer] := Module[
  {n, k, bitovi, kandidat},
  n = b - a + 1;
  k = Ceiling[Log[2, n]];
  kandidat = n;
  While[kandidat >= n,
    bitovi = BBSSljedeciBitovi[k];
    kandidat = FromDigits[bitovi, 2]
  ];
  a + kandidat
]

BBSBroj[k_Integer] := Module[{bitovi, broj},
  bitovi = BBSSljedeciBitovi[k];
  broj = FromDigits[bitovi, 2];
  broj = BitOr[broj, 2^(k - 1)];
  broj
]

EratostenovoSito[n_Integer] := Module[
  {ostalo, p, pm, i, brojProstih, rezultat, indeks},
  ostalo = Table[True, {n}];
  ostalo[[1]] = False;
  p = 2;
  While[p <= Floor[Sqrt[n]],
    pm = 2*p;
    While[pm <= n,
      ostalo[[pm]] = False;
      pm = pm + p
    ];
    i = p + 1;
    While[i <= n && !ostalo[[i]], i++];
    p = i
  ];
  brojProstih = 0;
  Do[If[ostalo[[i]], brojProstih++], {i, 2, n}];
  rezultat = Table[0, {brojProstih}];
  indeks = 1;
  Do[
    If[ostalo[[i]],
      rezultat[[indeks]] = i;
      indeks++
    ],
    {i, 2, n}
  ];
  rezultat
]

SlucajanProst[m_Integer, k_Integer : 20, B_Integer : 10000] := Module[
  {L, kandidat, djeljivo, faktor, prost},
  L = EratostenovoSito[B];
  prost = False;
  While[!prost,
    kandidat = BBSBroj[m];
    If[EvenQ[kandidat], kandidat = kandidat + 1];
    djeljivo = False;
    Do[
      If[faktor^2 > kandidat, Break[]];
      If[Mod[kandidat, faktor] == 0, djeljivo = True; Break[]],
      {faktor, L}
    ];
    If[!djeljivo, prost = MillerRabinTest[kandidat, k]]
  ];
  kandidat
]

SljedeciProst[n_Integer, k_Integer : 20, B_Integer : 10000] := Module[
  {L, p, djeljivo, faktor},
  L = EratostenovoSito[B];
  p = If[OddQ[n], n + 2, n + 1];
  While[True,
    djeljivo = False;
    Do[
      If[faktor^2 > p, Break[]];
      If[Mod[p, faktor] == 0, djeljivo = True; Break[]],
      {faktor, L}
    ];
    If[!djeljivo && MillerRabinTest[p, k], Return[p]];
    p = p + 2
  ]
]

SlucajanProstBrojPokusaja[m_Integer, k_Integer : 20, B_Integer : 10000] := Module[
  {L, kandidat, djeljivo, faktor, prost, ukupno, poslanoNaMR},
  L = EratostenovoSito[B];
  prost = False;
  ukupno = 0;
  poslanoNaMR = 0;
  While[!prost,
    ukupno++;
    kandidat = BBSBroj[m];
    If[EvenQ[kandidat], kandidat = kandidat + 1];
    djeljivo = False;
    Do[
      If[faktor^2 > kandidat, Break[]];
      If[Mod[kandidat, faktor] == 0, djeljivo = True; Break[]],
      {faktor, L}
    ];
    If[!djeljivo,
      poslanoNaMR++;
      prost = MillerRabinTest[kandidat, k]
    ]
  ];
  {kandidat, ukupno, poslanoNaMR}
]

FaktorizacijaDijeljenjem[N_Integer] := Module[
  {n = N, k, dk, q, r, e, rezultat},
  k = 0;
  rezultat = {};
  While[n != 1,
    dk = If[k == 0, 2, 2*k + 1];
    q = Quotient[n, dk];
    r = Mod[n, dk];
    If[r == 0,
      e = 0;
      While[Mod[n, dk] == 0,
        n = n/dk;
        e++
      ];
      AppendTo[rezultat, {dk, e}],

      If[q > dk,
        k++,
        AppendTo[rezultat, {n, 1}];
        Break[]
      ]
    ]
  ];
  rezultat
]

FermatovaFaktorizacija[n_Integer] := Module[
  {x0, y0, d, sy0, x, y, p, q},
  x0 = Floor[Sqrt[n]] + 1;
  y0 = x0^2 - n;
  d = 1;
  While[True,
    sy0 = Floor[Sqrt[y0]];
    If[sy0^2 == y0,
      x = Floor[Sqrt[n + y0]];
      y = Floor[Sqrt[y0]];
      q = x - y;
      p = x + y;
      Return[{p, q}],

      y0 = y0 + 2*x0 + d;
      d = d + 2;
      If[Floor[Sqrt[y0]] >= n/2,
        Return["Nisu prona\:0111eni netrivijalni faktori."]
      ]
    ]
  ]
]

PollardovaPMinus1[n_Integer, Bmin_Integer, Bmax_Integer, a_Integer : 2] := Module[
  {B, L, k, e, t, d},
  B = Bmin;
  While[B <= Bmax,
    L = EratostenovoSito[B];
    k = 1;
    Do[
      e = 0;
      While[p^(e + 1) <= B, e++];
      k = k*p^e,
      {p, L}
    ];
    t = ModPotenciranje[a, k, n];
    d = nzd[t - 1, n];
    If[1 < d < n, Return[d]];
    B++
  ];
  "Neuspjeh. Pove\[CAcute]ati B i/ili promijeniti a."
]

PollardovaPMinus1Prosireno[n_Integer, Bmin_Integer, Bmax_Integer, aMin_Integer : 2, aMax_Integer : 5] := Module[
  {B, L, k, e, t, d, a},
  B = Bmin;
  While[B <= Bmax,
    L = EratostenovoSito[B];
    k = 1;
    Do[
      e = 0;
      While[p^(e + 1) <= B, e++];
      k = k*p^e,
      {p, L}
    ];
    a = aMin;
    While[a <= aMax,
      t = ModPotenciranje[a, k, n];
      d = nzd[t - 1, n];
      If[1 < d < n, Return[d]];
      a++
    ];
    B++
  ];
  "Neuspjeh. Pro\[SHacek]ireni opseg za B i a nije dao netrivijalni djelilac."
]

PollardovaRho[n_Integer, x0_Integer : 2, c_Integer : 1] := Module[
  {x, k, l, p},
  x[0] = x0;
  k = 1;
  While[True,
    x[k] = Mod[x[k - 1]^2 + c, n];
    l = 0;
    While[l <= k - 1,
      p = nzd[x[k] - x[l], n];
      If[1 < p < n, Return[p]];
      If[p == n, Return["Neuspjeh. Promijeniti c i/ili x0."]];
      l++
    ];
    k++
  ]
]

PollardovaRhoFloyd[n_Integer, x0_Integer : 2, c_Integer : 1] := Module[
  {x, y, p},
  x = x0; y = x0;
  While[True,
    x = Mod[x^2 + c, n];
    y = Mod[y^2 + c, n];
    y = Mod[y^2 + c, n];
    p = nzd[x - y, n];
    If[1 < p < n, Return[p]];
    If[p == n, Return["Neuspjeh. Promijeniti c i/ili x0."]];
  ]
]

PollardovaRhoBrent[n_Integer, x0_Integer : 2, c_Integer : 1, m_Integer : 32] := Module[
  {y, r, q, G, x, k, ys},
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
        {Min[m, r - k]}
      ];
      G = nzd[q, n];
      k += m
    ];
    r *= 2
  ];
  If[G == n,
    While[True,
      ys = Mod[ys^2 + c, n];
      G = nzd[Abs[x - ys], n];
      If[G > 1, Break[]]
    ]
  ];
  If[G == n,
    "Neuspjeh. Promijeniti c i/ili x0.",
    G
  ]
]

RastaviNaProsteFaktore[n_Integer] := Module[
  {maliProsti, i, p, d, nadjen},
  If[n == 1, Return[{}]];
  If[MillerRabinTest[n, 20], Return[{n}]];
  maliProsti = EratostenovoSito[10000];
  nadjen = False;
  i = 1;
  While[i <= Length[maliProsti] && !nadjen,
    p = maliProsti[[i]];
    If[Mod[n, p] == 0, nadjen = True, i++]
  ];
  If[nadjen,
    Join[{p}, RastaviNaProsteFaktore[n/p]],
    (
      d = PollardovaRho[n];
      If[IntegerQ[d] && 1 < d < n,
        Join[RastaviNaProsteFaktore[d], RastaviNaProsteFaktore[n/d]],
        {n}
      ]
    )
  ]
]

PotpunaFaktorizacija[n_Integer] := Tally[RastaviNaProsteFaktore[n]]

EulerovaFunkcija[n_Integer] := Module[
  {faktorizacija, phi, i, p},
  faktorizacija = PotpunaFaktorizacija[n];
  phi = n;
  For[i = 1, i <= Length[faktorizacija], i++,
    p = faktorizacija[[i, 1]];
    phi = phi*(1 - 1/p)
  ];
  phi
]

MetodaFaktorskeBaze[n_Integer, w_Integer, maksPokusaja_Integer : 20000] := Module[
  {B, r, R, brojPokusaja, b, y, yAbs, ostatak, eksponenti, i, p, vektor,
   M, ns, koeficijenti, S, x, sumEksp, j, yKorijen, d},
  B = Join[{-1}, EratostenovoSito[w]];
  r = Length[B];
  R = {};
  brojPokusaja = 0;
  SeedRandom[Method -> "OpenSSL"];
  While[Length[R] <= r && brojPokusaja < maksPokusaja,
    brojPokusaja++;
    b = RandomInteger[{2, n - 2}];
    y = Mod[b^2, n];
    yAbs = If[y > n/2, y - n, y];
    ostatak = Abs[yAbs];
    eksponenti = Table[0, {r}];
    If[yAbs < 0, eksponenti[[1]] = 1];
    Do[
      p = B[[i]];
      While[Mod[ostatak, p] == 0, ostatak = ostatak/p; eksponenti[[i]]++],
      {i, 2, r}
    ];
    If[ostatak == 1,
      vektor = Mod[eksponenti, 2];
      AppendTo[R, {b, eksponenti, vektor}]
    ]
  ];
  If[Length[R] <= r, Return["Neuspjeh. Nedovoljno relacija u zadanom broju poku\[SHacek]aja."]];
  M = R[[All, 3]];
  ns = NullSpace[Transpose[M], Modulus -> 2];
  If[ns === {}, Return["Neuspjeh. Nema netrivijalnog rje\[SHacek]enja sistema."]];
  koeficijenti = First[ns];
  S = Flatten[Position[koeficijenti, _?OddQ]];
  If[S === {}, Return["Neuspjeh. Nema netrivijalnog rje\[SHacek]enja sistema."]];
  x = Mod[Times @@ (R[[#, 1]] & /@ S), n];
  sumEksp = Total[R[[#, 2]] & /@ S];
  yKorijen = Mod[Product[B[[j]]^(sumEksp[[j]]/2), {j, 2, r}], n];
  If[Mod[x - yKorijen, n] != 0 && Mod[x + yKorijen, n] != 0,
    d = nzd[Abs[x - yKorijen], n];
    If[1 < d < n, Return[d]]
  ];
  "Neuspjeh. Trivijalna kombinacija (x \[Congruent] \[PlusMinus]y mod n)."
]

DixonovAlgoritam[n_Integer, Bbound_Integer, maksPokusaja_Integer : 20000] := Module[
  {B, k, R, brojPokusaja, r, z, ostatak, eksponenti, i, p, vektor,
   M, ns, koeficijenti, S, x, sumEksp, j, yKorijen, d},
  B = EratostenovoSito[Bbound];
  k = Length[B];
  R = {};
  brojPokusaja = 0;
  SeedRandom[Method -> "OpenSSL"];
  While[Length[R] <= k && brojPokusaja < maksPokusaja,
    brojPokusaja++;
    r = RandomInteger[{2, n - 2}];
    z = Mod[r^2, n];
    ostatak = z;
    eksponenti = Table[0, {k}];
    Do[
      p = B[[i]];
      While[Mod[ostatak, p] == 0, ostatak = ostatak/p; eksponenti[[i]]++],
      {i, 1, k}
    ];
    If[ostatak == 1,
      vektor = Mod[eksponenti, 2];
      AppendTo[R, {r, eksponenti, vektor}]
    ]
  ];
  If[Length[R] <= k, Return["Neuspjeh. Nedovoljno relacija u zadanom broju poku\[SHacek]aja."]];
  M = R[[All, 3]];
  ns = NullSpace[Transpose[M], Modulus -> 2];
  If[ns === {}, Return["Neuspjeh. Nema netrivijalnog rje\[SHacek]enja sistema."]];
  koeficijenti = First[ns];
  S = Flatten[Position[koeficijenti, _?OddQ]];
  If[S === {}, Return["Neuspjeh. Nema netrivijalnog rje\[SHacek]enja sistema."]];
  x = Mod[Times @@ (R[[#, 1]] & /@ S), n];
  sumEksp = Total[R[[#, 2]] & /@ S];
  yKorijen = Mod[Product[B[[j]]^(sumEksp[[j]]/2), {j, k}], n];
  d = nzd[Abs[x - yKorijen], n];
  If[1 < d < n, Return[d]];
  "Neuspjeh."
]

ValidnaKrivulja[a_Integer, b_Integer, p_Integer] := Mod[4*a^3 + 27*b^2, p] != 0

TackaNaKrivulji[x_Integer, y_Integer, a_Integer, b_Integer, p_Integer] := Mod[y^2, p] == Mod[x^3 + a*x + b, p]

SabiranjeTacaka[P_, Q_, a_Integer, p_Integer] := Module[
  {x1, y1, x2, y2, lambda, x3, y3},
  If[P === O, Return[Q]];
  If[Q === O, Return[P]];
  x1 = P[[1]]; y1 = P[[2]];
  x2 = Q[[1]]; y2 = Q[[2]];
  If[x1 == x2 && Mod[y1 + y2, p] == 0, Return[O]];
  If[x1 == x2 && y1 == y2,
    lambda = Mod[(3*x1^2 + a)*MultiplikativniInverz[Mod[2*y1, p], p], p],
    lambda = Mod[(y2 - y1)*MultiplikativniInverz[Mod[x2 - x1, p], p], p]
  ];
  x3 = Mod[lambda^2 - x1 - x2, p];
  y3 = Mod[lambda*(x1 - x3) - y1, p];
  {x3, y3}
]

VisekratnikTacke[k_Integer, P_, a_Integer, p_Integer] := Module[
  {bitovi, Q, bit},
  bitovi = IntegerDigits[k, 2];
  Q = O;
  Do[
    Q = SabiranjeTacaka[Q, Q, a, p];
    If[bit == 1, Q = SabiranjeTacaka[Q, P, a, p]],
    {bit, bitovi}
  ];
  Q
]

RedTacke[P_, a_Integer, p_Integer] := Module[
  {Q, k},
  Q = P;
  k = 1;
  While[Q =!= O,
    Q = SabiranjeTacaka[Q, P, a, p];
    k++
  ];
  k
]

RedKrivuljeBruteForce[a_Integer, b_Integer, p_Integer] := 1 + p + Sum[JacobijevSimbol[x^3 + a*x + b, p], {x, 0, p - 1}]

RedKrivuljeShanksMestre[a_Integer, b_Integer, p_Integer, P_] := Module[
  {m, floorSqrt, Q, R, negR, malaKoraci, velikiKoraci, kandidati, i, j, t},
  m = Ceiling[2*p^(1/4)];
  floorSqrt = Floor[2*Sqrt[p]];
  Q = VisekratnikTacke[p + 1 + floorSqrt, P, a, p];
  R = VisekratnikTacke[m, P, a, p];
  negR = If[R === O, O, {R[[1]], Mod[-R[[2]], p]}];
  malaKoraci = Table[VisekratnikTacke[j, P, a, p], {j, 0, m - 1}];
  velikiKoraci = Table[SabiranjeTacaka[Q, VisekratnikTacke[i, negR, a, p], a, p], {i, 0, m - 1}];
  kandidati = {};
  Do[
    Do[
      If[velikiKoraci[[i + 1]] === malaKoraci[[j + 1]],
        t = i*m + j - floorSqrt;
        AppendTo[kandidati, p + 1 - t]
      ],
      {j, 0, m - 1}
    ],
    {i, 0, m - 1}
  ];
  kandidati = DeleteDuplicates[kandidati];
  If[Length[kandidati] == 1,
    First[kandidati],
    {"Neuspjeh. Kandidat nije jedinstven, probati novu ta\[CHacek]ku P", kandidati}
  ]
]

RedKrivulje[a_Integer, b_Integer, p_Integer, P_ : Null] := If[
  p <= 10^4,
  RedKrivuljeBruteForce[a, b, p],
  If[P === Null,
    "Neuspjeh. Za p>10^4 potrebno je zadati ta\[CHacek]ku P (Shanks-Mestreova metoda).",
    RedKrivuljeShanksMestre[a, b, p, P]
  ]
]

GeneratoriKrivulje[a_Integer, b_Integer, p_Integer] := Module[
  {red, sveTacke, x, y, rhs},
  red = RedKrivuljeBruteForce[a, b, p];
  sveTacke = {};
  Do[
    rhs = Mod[x^3 + a*x + b, p];
    Do[
      If[Mod[y^2, p] == rhs, AppendTo[sveTacke, {x, y}]],
      {y, 0, p - 1}
    ],
    {x, 0, p - 1}
  ];
  Select[sveTacke, RedTacke[#, a, p] == red &]
]

HasseProvjera[p_Integer, red_Integer] := p + 1 - 2*Sqrt[p] <= red <= p + 1 + 2*Sqrt[p]

RSAGenerisiKljuceve[m_Integer, k_Integer : 20] := Module[
  {p, q, n, phi, e, d},
  BBSPokreni[m, k];
  p = SlucajanProst[m, k];
  q = SlucajanProst[m, k];
  While[p == q, q = SlucajanProst[m, k]];
  n = p*q;
  phi = (p - 1)*(q - 1);
  e = BBSBrojIzOpsega[10^5 + 1, phi - 1];
  While[nzd[e, phi] != 1, e = BBSBrojIzOpsega[10^5 + 1, phi - 1]];
  d = MultiplikativniInverz[e, phi];
  {n, e, d, p, q}
]

RSAProvjeriKljuceve[n_Integer, e_Integer, d_Integer, p_Integer, q_Integer, k_Integer : 20] := Module[
  {phi, greske},
  phi = (p - 1)*(q - 1);
  greske = {};
  If[n != p*q, AppendTo[greske, "n nije jednako p*q"]];
  If[!MillerRabinTest[p, k], AppendTo[greske, "p nije prost"]];
  If[!MillerRabinTest[q, k], AppendTo[greske, "q nije prost"]];
  If[nzd[e, phi] != 1, AppendTo[greske, "nzd(e,phi(n)) nije 1"]];
  If[Mod[d*e, phi] != 1, AppendTo[greske, "d*e nije kongruentno 1 mod phi(n)"]];
  If[greske == {}, True, greske]
]

RSASifriraj[x_Integer, n_Integer, e_Integer] := ModPotenciranje[x, e, n]

RSADesifriraj[y_Integer, n_Integer, d_Integer] := ModPotenciranje[y, d, n]

RabinGenerisiKljuceve[m_Integer, k_Integer : 20] := Module[
  {p, q},
  BBSPokreni[m, k];
  p = SlucajanProst[m, k];
  While[Mod[p, 4] != 3, p = SlucajanProst[m, k]];
  q = SlucajanProst[m, k];
  While[q == p || Mod[q, 4] != 3, q = SlucajanProst[m, k]];
  {p*q, p, q}
]

RabinProvjeriKljuceve[n_Integer, p_Integer, q_Integer, k_Integer : 20] := Module[
  {greske},
  greske = {};
  If[n != p*q, AppendTo[greske, "n nije jednako p*q"]];
  If[!MillerRabinTest[p, k], AppendTo[greske, "p nije prost"]];
  If[!MillerRabinTest[q, k], AppendTo[greske, "q nije prost"]];
  If[Mod[p, 4] != 3, AppendTo[greske, "p nije kongruentno 3 mod 4"]];
  If[Mod[q, 4] != 3, AppendTo[greske, "q nije kongruentno 3 mod 4"]];
  If[greske == {}, True, greske]
]

RabinSifriraj[x_Integer, n_Integer] := Mod[x^2, n]

RabinDesifriraj[c_Integer, p_Integer, q_Integer] := Module[
  {n, gab, a, b, r, s, x, y},
  n = p*q;
  gab = ProsireniEuklid[p, q];
  a = gab[[2]]; b = gab[[3]];
  r = ModPotenciranje[c, (p + 1)/4, p];
  s = ModPotenciranje[c, (q + 1)/4, q];
  x = Mod[a*p*s + b*q*r, n];
  y = Mod[a*p*s - b*q*r, n];
  {x, Mod[-x, n], y, Mod[-y, n]}
]

DHGenerisiParametre[m_Integer, k_Integer : 20] := Module[
  {p, faktori, g},
  BBSPokreni[m, k];
  p = SlucajanProst[m, k];
  faktori = PotpunaFaktorizacija[p - 1];
  g = PrimitivniKorijen[p, faktori];
  {p, g}
]

DHProvjeriParametre[p_Integer, g_Integer, k_Integer : 20] := Module[
  {greske, faktori, red},
  greske = {};
  If[!MillerRabinTest[p, k], AppendTo[greske, "p nije prost"]];
  If[greske == {},
    faktori = PotpunaFaktorizacija[p - 1];
    red = RedElementa[g, p, faktori];
    If[red != p - 1, AppendTo[greske, "g nije primitivni korijen (generator) od p"]]
  ];
  If[greske == {}, True, greske]
]

DHGenerisiKljuc[p_Integer, g_Integer] := Module[
  {a, A},
  a = BBSBrojIzOpsega[1, p - 1];
  A = ModPotenciranje[g, a, p];
  {a, A}
]

DHZajednickiKljuc[strana_Integer, tajni_Integer, p_Integer] := ModPotenciranje[strana, tajni, p]

ElGamalGenerisiKljuceve[m_Integer, k_Integer : 20] := Module[
  {p, faktori, alpha, a, beta},
  BBSPokreni[m, k];
  p = SlucajanProst[m, k];
  faktori = PotpunaFaktorizacija[p - 1];
  alpha = PrimitivniKorijen[p, faktori];
  a = BBSBrojIzOpsega[0, p - 2];
  beta = ModPotenciranje[alpha, a, p];
  {p, alpha, a, beta}
]

ElGamalProvjeriKljuceve[p_Integer, alpha_Integer, a_Integer, beta_Integer, k_Integer : 20] := Module[
  {greske, faktori, red},
  greske = {};
  If[!MillerRabinTest[p, k], AppendTo[greske, "p nije prost"]];
  If[greske == {},
    faktori = PotpunaFaktorizacija[p - 1];
    red = RedElementa[alpha, p, faktori];
    If[red != p - 1, AppendTo[greske, "alpha nije primitivni korijen (generator) od p"]]
  ];
  If[ModPotenciranje[alpha, a, p] != beta, AppendTo[greske, "beta nije jednako alpha^a mod p"]];
  If[greske == {}, True, greske]
]

ElGamalSifriraj[x_Integer, p_Integer, alpha_Integer, beta_Integer] := Module[
  {k, y1, y2},
  k = BBSBrojIzOpsega[0, p - 2];
  y1 = ModPotenciranje[alpha, k, p];
  y2 = Mod[x*ModPotenciranje[beta, k, p], p];
  {y1, y2}
]

ElGamalDesifriraj[y1_Integer, y2_Integer, p_Integer, a_Integer] := Module[
  {inv},
  inv = MultiplikativniInverz[ModPotenciranje[y1, a, p], p];
  Mod[y2*inv, p]
]

MVGenerisiKljuceve[m_Integer, k_Integer : 20] := Module[
  {p, a, b, x, y, rhs, alpha, redH, mTajni, beta, found},
  BBSPokreni[m, k];
  p = SlucajanProst[m, k];
  While[True,
    a = BBSBrojIzOpsega[0, p - 1];
    b = BBSBrojIzOpsega[0, p - 1];
    If[ValidnaKrivulja[a, b, p], Break[]]
  ];
  found = False;
  While[!found,
    x = BBSBrojIzOpsega[0, p - 1];
    rhs = Mod[x^3 + a*x + b, p];
    If[JacobijevSimbol[rhs, p] != -1,
      y = 0;
      While[y < p && Mod[y^2, p] != rhs, y++];
      If[y < p, found = True; alpha = {x, y}]
    ]
  ];
  redH = RedTacke[alpha, a, p];
  mTajni = BBSBrojIzOpsega[1, redH - 1];
  beta = VisekratnikTacke[mTajni, alpha, a, p];
  {p, a, b, alpha, redH, mTajni, beta}
]

MVProvjeriKljuceve[p_Integer, a_Integer, b_Integer, alpha_, redH_Integer, mTajni_Integer, beta_] := Module[
  {greske},
  greske = {};
  If[!ValidnaKrivulja[a, b, p], AppendTo[greske, "krivulja nije validna (singularna)"]];
  If[!TackaNaKrivulji[alpha[[1]], alpha[[2]], a, b, p], AppendTo[greske, "alpha nije na krivulji"]];
  If[RedTacke[alpha, a, p] != redH, AppendTo[greske, "redH nije stvarni red od alpha"]];
  If[VisekratnikTacke[mTajni, alpha, a, p] != beta, AppendTo[greske, "beta nije jednako [m]alpha"]];
  If[greske == {}, True, greske]
]

MVSifriraj[{x1_Integer, x2_Integer}, a_Integer, p_Integer, alpha_, beta_, redH_Integer] := Module[
  {k, y0, c, c1, c2, y1, y2},
  While[True,
    k = BBSBrojIzOpsega[1, redH - 1];
    c = VisekratnikTacke[k, beta, a, p];
    If[c =!= O && c[[1]] != 0 && c[[2]] != 0, Break[]]
  ];
  y0 = VisekratnikTacke[k, alpha, a, p];
  c1 = c[[1]]; c2 = c[[2]];
  y1 = Mod[c1*x1, p];
  y2 = Mod[c2*x2, p];
  {y0, y1, y2}
]

MVDesifriraj[{y0_, y1_Integer, y2_Integer}, a_Integer, p_Integer, mTajni_Integer] := Module[
  {c, c1, c2, x1, x2},
  c = VisekratnikTacke[mTajni, y0, a, p];
  c1 = c[[1]]; c2 = c[[2]];
  x1 = Mod[y1*MultiplikativniInverz[c1, p], p];
  x2 = Mod[y2*MultiplikativniInverz[c2, p], p];
  {x1, x2}
]

End[]
EndPackage[]
