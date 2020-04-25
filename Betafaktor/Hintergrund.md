### Kurse und Renditen

Die diskrete Rendite eines Wertpapiers oder eines Index ist definiert als Quotient zwischen der Differenz der Schlusskurse $S$ des Tages $t$ und des Vortages $t-1$ und dem Kurs des Vortages:

$$r_{t(diskret)}=\frac{S_t-S_{t-1}}{S_{t-1}}.$$

Alternativ kann das logarithmierte Verhältnis der Schlusskurse genutzt werden (stetige Rendite):

$$r_{t(stetig)}=\ln\left(\frac{S_t}{S_{t-1}}\right)=ln(r_{t(diskret)}+1).$$

Bei kleinen Renditen unterscheiden sich die beiden Ansätze nicht wesentlich voneinander. Die stetige Rendite findet häufig aufgrund der mathematisch besseren Handhabbarkeit Anwendung.

### Capital Asset Pricing Modell (CAPM)

Das CAPM nach nach Sharpe (1964) (bzw. Lintner, 1965 oder Mossin, 1966) ist ein sogenanntes Einfaktoren-Modell zur Abschätzung der Rendite $r$ (bzw. des Aufschlags der Rendite auf die risikolose Verzinsung $r_f$) einer Aktie $i$ auf Basis der Rendite $r_m$ eines Marktportfolios:

$$r_{i,t}=r_f+\alpha+\beta_i\cdot(r_{m,t}-r_{f,t}).$$

$\beta_i$ ist ein Faktor, der das systematische, also das unternehmensspezifische Risiko des betreffenden Wertpapiers $i$ beinhaltet. 
Im Mittel ist das Beta des Marktes 1. 
Wertpapiere mit einem höheren Beta als 1 werden als riskanter (und chancenreicher) als der Markt angesehen, mit
einem niedrigerem Beta als 1 entsprechend als solide (und weniger chancenreich). 


### Schätzung des Beta-Faktors

In der Praxis wird das Beta häufig über eine lineare Regression der Renditen des interessierenden Wertpapiers auf die Rendite des Index eines Vergleichsmarktes geschätzt:

$$r_{i,t}=\alpha + \beta \cdot r_{m,t} + \epsilon_t$$

Hierbei kommen unterschiedlich Marktindices zum Einsatz, unterschiedliche Perioden für die Renditeberechnung (täglich, wöchentlich, monatlich) und unterschiedliche Längen des Schätzfensters.

