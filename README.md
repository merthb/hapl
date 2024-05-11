# HAPL — Haskell Plágiumellenőrző szoftver

## Rövid leírás

A szakdolgozat célja egy olyan plágiumellenőrző szoftver írása, amely Haskell programok háttérlogikája mentén ad egyezési százalékot. Ahhoz, hogy bármilyen program háttérlogikáját tudjuk vizsgálni, valamilyen módon el kell vonatkoztatni a szövegi síktól, és egy absztraktabb adatszerkezetben kell tekinteni a programra. [M. L. Kammer diplomamunkájában](https://webspace.science.uu.nl/~hage0101/downloads/marnixkammer-msc.pdf) erre függvényhívási gráfokat használt, így én is emellett döntöttem. 

A szoftver tehát elsősorban minden ellenőrizni kívánt programhoz felépíti a függvényhívási gráfot — azaz összeszedi, hogy az egyes függvények mely másik függvényeket hívják meg, a függvények lesznek a csúcsai a gráfnak, és a függvényhívások az irányított élek egyik függvénytől a másikhoz —, majd megkeresi két így előállított gráf közt a legrövidebb utat [A* algoritmus](https://theory.stanford.edu/~amitp/GameProgramming/AStarComparison.html) segítségével. Másképpen megfogalmazva részgráf egyezéseket keres, hiszen ahol a két gráf egyezik, azon szakaszokon az útnak költsége nincsen. Ezek után pedig megadja az egyezési százalékot a vélhető maximum költség és a talált legrövidebb út kölstégének különbsége által.
