import delimited "C:\Dev\_INSPER\IC_ENEM\sample\sample_stata.csv", bindquote(strict) stripquote(yes) 
* label define regiao 1 "N" 2 "NE" 3 "CO" 4 "SE" 5 "S"
* label define educ_mae 1 "fund_inc" 2 "fund" 3 "med" 4 "sup"
* label define escola 1 "publ" 0 "priv"
* label define raca 1 "pre" 0 "bra"
* label define sexo 1 "F" 0 "M"
* label define internet 1 "N" 0 "S"
label values regiao regiao
label values educ_mae educ_mae
label values escola escola
label values raca raca
label values internet internet
label values sexo sexo
set showbaselevels on

* regressao principal com interacoes e FEs de ano
reg nota i.(raca sexo educ_mae regiao escola internet)##i.ano

* regressao principal com interacoes e FEs de ano e municipio, erros padrao clusterizados por municipio
reghdfe nota i.(raca sexo educ_mae escola internet)##i.ano, absorb(i.muni_prova) vce(cl i.muni_prova)