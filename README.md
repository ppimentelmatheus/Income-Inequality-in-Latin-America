# Business Cycle and Inequality in Emerging Economies: Brazil HPV Replication

Status: projeto em andamento, baseline Brasil v1/v2 congelado e candidato Brasil v3 com bloco RAIS em avaliacao.

Este repositorio adapta o arcabouco de Heathcote, Perri e Violante (HPV) para o Brasil. O objetivo e estudar como ciclos economicos afetam desemprego, participacao, transicoes no mercado de trabalho e distribuicao da renda do trabalho usando microdados brasileiros, principalmente RAIS e PNAD Continua.

A versao atual combina tres blocos empiricos:

- RAIS: distribuicao da remuneracao formal para homens de 25 a 55 anos.
- PNAD-C: transicoes trimestrais entre emprego, desemprego e inatividade.
- Modelo Julia: adaptacao do codigo HPV para receber insumos brasileiros e comparar modelo versus dados.

O projeto ainda nao e uma replicacao final. A versao atual ja permite reproduzir as principais figuras brasileiras e avaliar o fit inicial do modelo, mas ainda restam etapas importantes de calibracao conjunta.

## Estrutura Do Projeto

```text
Propose-Paper/
├── README.md
├── text/
│   ├── document.tex
│   ├── document.pdf
│   └── figures/
├── Propose/
│   ├── R/
│   │   ├── RAIS/
│   │   └── PNADC/
│   └── replication/
│       ├── HPV code/
│       └── HPV Brazil code/
└── replication/
    └── arquivos originais/auxiliares da replicacao HPV
```

### `text/`

Contem o artigo em formato LaTeX. O arquivo principal e `document.tex`, e o PDF compilado e `document.pdf`. Essa pasta documenta a motivacao, o modelo, os dados, a calibracao brasileira, os resultados atuais e as limitacoes.

### `Propose/R/RAIS/`

Contem os scripts em R para coleta, leitura, tratamento e visualizacao dos dados da RAIS. Esse bloco foi usado para construir os percentis e razoes de percentis da remuneracao formal brasileira.

### `Propose/R/PNADC/`

Contem os scripts em R para baixar, organizar e estimar transicoes trimestrais da PNAD Continua. Essa pasta tambem contem configuracoes do ciclo economico brasileiro e os objetos exportados para Julia.

### `Propose/replication/HPV code/`

Contem o codigo original da replicacao de HPV. Essa pasta deve ser tratada como referencia: ela nao e a versao brasileira do projeto.

### `Propose/replication/HPV Brazil code/`

Contem a adaptacao brasileira do modelo em Julia. Essa e a pasta principal para rodar o modelo, preparar insumos, calibrar momentos e gerar figuras comparando modelo e dados brasileiros.

## O Que Foi Feito Ate Aqui

## 1. Bloco RAIS

O primeiro bloco empirico construiu a distribuicao de renda formal a partir da RAIS.

Foram implementados scripts para:

- ler arquivos da RAIS a partir de 1994;
- identificar as variaveis relevantes nos layouts antigos e novos;
- filtrar homens entre 25 e 55 anos;
- trabalhar com remuneracao media em multiplos do salario minimo;
- calcular os percentis P20, P50, P90 e P95;
- calcular as razoes P90/P50 e P50/P20;
- construir series em indice com base 1994 igual a 100;
- construir series em desvios logaritmicos relativos a 1994;
- destacar anos com dados parciais a partir de 2018;
- gerar graficos inspirados nas figuras de HPV.

Uma preocupacao central foi evitar o processamento integral de todos os arquivos da RAIS, pois isso ficou pesado para o SSD e para a memoria disponivel. Por isso, a solucao evoluiu para uma rotina mais leve, baseada em leitura seletiva e processamento incremental.

Resultado economico importante: os percentis da RAIS em multiplos do salario minimo caem em relacao a 1994 em parte porque a medida esta normalizada pelo salario minimo. Portanto, a queda indica reducao do numero de salarios minimos recebidos em cada percentil, nao necessariamente queda da renda real.

Figuras principais geradas nesse bloco:

- percentis da distribuicao de renda formal, Brasil, RAIS;
- razao P90/P50;
- razao P50/P20;
- comparacoes temporais dos percentis e das razoes.

## 2. Bloco PNAD Continua

O segundo bloco empirico passou a usar a PNAD Continua para estimar transicoes no mercado de trabalho. A RAIS e adequada para salarios formais, mas nao observa desemprego e inatividade. Por isso, as transicoes do modelo precisam vir da PNAD-C.

Foram implementados scripts para:

- baixar microdados trimestrais da PNAD-C;
- organizar os arquivos por ano e trimestre;
- parear individuos em trimestres adjacentes;
- classificar estados laborais em emprego, desemprego e inatividade;
- estimar matrizes de transicao entre estados;
- estimar transicoes por fase do ciclo economico;
- estimar transicoes por grupo educacional;
- exportar objetos de transicao para uso no modelo Julia.

O periodo de trabalho da PNAD-C cobre a janela disponivel a partir de 2012. A coleta teve problemas pontuais por espaco em disco em 2022, mas a estrutura foi preparada para retomar e completar os trimestres faltantes.

### Estados Do Mercado De Trabalho

O modelo trabalha com tres estados laborais observados na PNAD-C:

- `E`: empregado;
- `U`: desempregado;
- `N`: fora da forca de trabalho.

As matrizes estimadas capturam probabilidades trimestrais de transicao entre esses estados.

### Estados Do Ciclo Economico

Para classificar o ciclo economico brasileiro, foi definido que o PIB deve vir sempre do SIDRA, usando PIB a precos de mercado.

A classificacao atual e:

- `Crisis`: anos de 2015, 2016 e 2020;
- `Recession`: trimestres nao classificados como crise com crescimento negativo do PIB;
- `Expansion`: trimestres nao classificados como crise com crescimento do PIB entre 0% e 1%;
- `Boom`: trimestres nao classificados como crise com crescimento do PIB acima de 1%.

Essa classificacao substitui, na pratica atual, uma aplicacao mecanica da datacao CODACE. A CODACE continua sendo uma referencia possivel para robustez.

### Grupos De Habilidade

Inicialmente o projeto separava trabalhadores em dois grupos: low skill e high skill. Depois, para melhorar a adaptacao ao Brasil, o ensino medio completo foi separado como um terceiro grupo.

A classificacao atual e:

- `LowSkill`: baixa escolaridade, abaixo do ensino medio completo;
- `MidSkill`: ensino medio completo;
- `HighSkill`: ensino superior incompleto ou completo.

Essa mudanca e importante porque o ensino medio completo representa uma parcela grande da forca de trabalho brasileira e nao deve ser automaticamente tratado como baixa habilidade ou alta habilidade.

## 3. Adaptacao Do Modelo HPV Para O Brasil

Foi criada uma nova pasta para a versao brasileira do modelo:

```text
Propose/replication/HPV Brazil code/
```

Essa pasta parte da estrutura do codigo original de HPV, mas altera os insumos e partes da calibracao para o Brasil.

Principais arquivos:

- `hpv_v3_brazil.jl`: arquivo principal do modelo brasileiro;
- `inputs/brazil_calibration.jl`: parametros brasileiros usados pelo modelo;
- `inputs/brazil_transition_objects.jl`: objetos de transicao agregados;
- `inputs/brazil_transition_objects_by_skill.jl`: objetos de transicao por grupo educacional;
- `scripts/prepare_brazil_inputs.jl`: prepara insumos brasileiros para Julia;
- `scripts/plot_model_vs_data.jl`: gera comparacoes entre modelo e dados;
- `scripts/calibrate_labor_moments.jl`: calibra momentos de desemprego e participacao;
- `scripts/evaluate_rais_wage_fit.jl`: avalia o fit do bloco RAIS;
- `scripts/calibrate_rais_wage_moments.jl`: busca parametros do bloco salarial;
- `scripts/plot_transition_probabilities_by_skill.jl`: gera graficos de job loss e job finding por habilidade.

Tambem foi escrito um documento comparando o codigo original e a adaptacao brasileira:

```text
Propose/replication/HPV Brazil code/docs/original_vs_brazil.md
```

## 4. Separacao Entre RAIS E Earnings Com Zeros

Uma correcao conceitual importante foi separar dois objetos:

- `RAIS`: distribuicao salarial condicional a estar empregado no setor formal;
- `earnings com zeros`: distribuicao de renda incluindo nao emprego como zero.

Essa separacao foi necessaria porque a RAIS nao observa trabalhadores desempregados ou fora da forca de trabalho. Comparar diretamente uma distribuicao do modelo com zeros contra uma distribuicao RAIS contaminava a interpretacao dos percentis.

A partir dessa correcao, o modelo passou a produzir tambem saidas condicionais a empregado, permitindo comparacao mais apropriada com os percentis da RAIS.

## 5. Calibracao Do Bloco De Trabalho

Foi feita uma calibracao inicial do bloco de desemprego e participacao, buscando aproximar os momentos da PNAD-C.

Parametros ajustados:

- `kappa_e`;
- `scale_adjustment`;
- `phi_bar_1967`;
- `v_phi`;
- `lambda`.

A calibracao atual usa:

```text
kappa_e = 0.65
scale_adjustment = [1.081119462, 1.311346475, 1.364307576, 1.455863855]
phi_bar_1967 = -0.85
v_phi = 0.08
lambda = 0.30
```

Com essa configuracao, os resultados agregados atuais sao aproximadamente:

```text
Desemprego medio no modelo: 0.0656
Desemprego medio PNAD-C:    0.0616
RMSE desemprego:            0.0158

Participacao media modelo:  0.8673
Participacao media PNAD-C:  0.9003
RMSE participacao:          0.0343
```

O desemprego ja esta razoavelmente proximo dos dados brasileiros. A participacao ainda esta abaixo da PNAD-C, especialmente depois da calibracao do bloco salarial.

## 6. Calibracao Do Bloco Salarial RAIS

Depois de estabilizar o bloco de trabalho, o projeto passou para o bloco de salarios e habilidades usando RAIS. A ideia foi ajustar os parametros do processo salarial para aproximar as trajetorias dos percentis e das razoes de percentis.

A calibracao candidata atual e chamada de `trial 6`.

Parametros do candidato atual:

```text
sigma_1967   = 0.7546742149938931
gamma_sigma  = 0.001136224478824172
gamma_s      = 0.00014177510365752624
v_epsilon    = 0.008883140407741769
delta_plus   = 0.004908851148272356
delta_minus  = 0.06466203277316082
```

Pesos educacionais usados:

```text
LowSkill  = 0.4642887907
MidSkill  = 0.3250592613
HighSkill = 0.2106519479
```

Fit atual do bloco RAIS:

```text
P20 index RMSE: 4.728   | modelo final:  94.94 | RAIS final:  95.90
P50 index RMSE: 3.441   | modelo final: 101.15 | RAIS final:  98.90
P90 index RMSE: 7.134   | modelo final: 113.27 | RAIS final: 122.39
P95 index RMSE: 8.222   | modelo final: 117.55 | RAIS final: 127.08
P90/P50 RMSE:  0.2816
P50/P20 RMSE:  0.4800
```

O candidato atual melhora a parte superior da distribuicao, mas ainda nao reproduz perfeitamente a razao P50/P20 e tambem piora parte do ajuste de participacao. Portanto, ele e um candidato promissor, mas nao a calibracao final.

## 7. Figuras De Transicao Por Habilidade

Tambem foram criadas figuras semelhantes a figura 7 do artigo original, comparando modelo e dados para:

- probabilidade de perda de emprego por nivel de habilidade;
- probabilidade de encontrar emprego por nivel de habilidade.

Arquivos gerados:

```text
Propose/replication/HPV Brazil code/output/transition_probabilities_by_skill_model_vs_data.csv
Propose/replication/HPV Brazil code/figures/job_loss_probability_by_skill_model_vs_data.png
Propose/replication/HPV Brazil code/figures/job_finding_probability_by_skill_model_vs_data.png
Propose/replication/HPV Brazil code/figures/transition_probabilities_by_skill_model_vs_data.png
```

Conclusao preliminar desse bloco:

- trabalhadores low-skill tem maior risco de perda de emprego;
- trabalhadores high-skill sao mais protegidos no emprego;
- o modelo preserva o ranking por habilidade;
- a escala de job loss no modelo ainda e suavizada por `kappa_e = 0.65`;
- o modelo aumenta a probabilidade de job finding por meio de `scale_adjustment`, especialmente em booms;
- a versao atual ainda usa grupos educacionais discretos, nao um perfil continuo de habilidade como no espirito completo de HPV.

## Como Reproduzir A Versao Atual

Os caminhos abaixo devem ser executados a partir da raiz do projeto ou da pasta indicada.

### 1. RAIS

Entre na pasta da RAIS e siga o README especifico:

```bash
cd "Propose/R/RAIS"
```

O pipeline RAIS deve ser usado principalmente para atualizar os objetos agregados e as figuras de percentis. Evite reprocessar todos os arquivos brutos sem necessidade, pois a RAIS completa e pesada.

### 2. PNAD-C

Entre na pasta da PNAD-C e siga o README especifico:

```bash
cd "Propose/R/PNADC"
```

Esse bloco baixa os microdados, estima transicoes trimestrais, classifica fases do ciclo e exporta os objetos usados no modelo.

### 3. Preparar Insumos Julia

```bash
cd "Propose/replication/HPV Brazil code"
julia --project=. scripts/prepare_brazil_inputs.jl
```

### 4. Rodar O Modelo Brasil Com O Candidato Atual

```bash
cd "Propose/replication/HPV Brazil code"

HPV_BR_SIGMA_1967=0.7546742149938931 \
HPV_BR_GAMMA_SIGMA=0.001136224478824172 \
HPV_BR_GAMMA_S=0.00014177510365752624 \
HPV_BR_V_EPSILON=0.008883140407741769 \
HPV_BR_DELTA_PLUS=0.004908851148272356 \
HPV_BR_DELTA_MINUS=0.06466203277316082 \
julia --project=. hpv_v3_brazil.jl
```

A ultima rodada completa com esses parametros levou cerca de 56 minutos nesta maquina.

### 5. Gerar Diagnosticos E Figuras

```bash
cd "Propose/replication/HPV Brazil code"

julia --project=. scripts/evaluate_rais_wage_fit.jl
julia --project=. scripts/plot_model_vs_data.jl
julia --project=. scripts/plot_transition_probabilities_by_skill.jl
```

### 6. Compilar O Documento

```bash
cd "text"
pdflatex -interaction=nonstopmode document.tex
pdflatex -interaction=nonstopmode document.tex
```

## Principais Resultados Ate Aqui

A versao Brasil v1/v2/v3 ainda e preliminar, mas ja permite algumas conclusoes.

Primeiro, a dinamica brasileira da distribuicao formal de salarios difere bastante do padrao norte-americano enfatizado por HPV. Na RAIS, quando os rendimentos sao medidos em multiplos do salario minimo e normalizados por 1994, ha forte compressao relativa da distribuicao formal. Isso nao deve ser lido mecanicamente como empobrecimento real, mas como reducao dos rendimentos formais em termos de salarios minimos.

Segundo, as transicoes da PNAD-C mostram uma hierarquia clara por escolaridade: trabalhadores com menor escolaridade enfrentam maior risco de perda de emprego, enquanto trabalhadores com maior escolaridade sao mais protegidos. O grupo de ensino medio completo e empiricamente relevante e precisa permanecer separado como `MidSkill`.

Terceiro, recessoes afetam renda e desigualdade por dois canais principais no modelo: maior risco de perda de emprego e menor retorno esperado da busca. Isso reduz renda esperada e pode afetar a decisao de participacao na forca de trabalho. Na versao atual, o modelo captura parte desses mecanismos, mas ainda precisa melhorar o ajuste conjunto entre desemprego, participacao e distribuicao salarial.

Quarto, o desemprego agregado esta relativamente bem ajustado. A participacao, no entanto, permanece baixa no modelo em relacao a PNAD-C depois da calibracao salarial candidata. Esse e um dos principais pontos a corrigir antes de declarar uma versao final.

## O Que Ainda Falta Trabalhar

## 1. Calibracao Conjunta

A principal limitacao atual e que desemprego, participacao e distribuicao salarial ainda nao foram estimados conjuntamente.

Proxima etapa recomendada:

- estimar `kappa_e`, `scale_adjustment`, `phi_bar`, `v_phi` e `lambda` minimizando uma funcao de perda conjunta para desemprego e participacao PNAD-C;
- manter o desemprego proximo dos dados sem derrubar demais a participacao;
- depois recalibrar os parametros salariais da RAIS condicionais ao emprego.

## 2. Refinar O Bloco Salarial RAIS

O candidato `trial 6` melhora a cauda superior, mas ainda precisa melhorar o ajuste da parte inferior da distribuicao.

Pontos pendentes:

- fazer busca local ao redor do `trial 6`;
- reduzir o erro da razao P50/P20;
- manter o bom ajuste de P90/P50;
- avaliar P95 e P90 sem piorar P20;
- testar pesos por habilidade e parametros salariais especificos por grupo educacional.

## 3. Transicoes Por Habilidade Mais Ricas

A versao atual usa grupos educacionais discretos: LowSkill, MidSkill e HighSkill. Isso ja melhora a adaptacao para o Brasil, mas ainda esta longe de um perfil continuo de habilidade.

Melhorias possiveis:

- estimar transicoes por decis de salario potencial;
- combinar educacao e posicao salarial inicial;
- permitir que job loss e job finding variem por habilidade continua;
- aproximar melhor a estrutura do modelo original de HPV.

## 4. Participacao E Margem Informal

A PNAD-C observa emprego formal e informal, enquanto a RAIS observa apenas o setor formal. Essa diferenca pode afetar a comparacao entre modelo e dados.

Melhorias possiveis:

- separar emprego formal e informal na PNAD-C;
- usar salarios PNAD-C como robustez;
- construir momentos comparaveis somente para ocupados formais;
- avaliar se a saida da formalidade esta sendo confundida com desemprego ou inatividade.

## 5. Deflacao E Unidade De Conta

Os graficos RAIS principais usam remuneracao em multiplos do salario minimo. Isso e informativo, mas nao substitui uma analise de renda real.

Melhorias possiveis:

- construir uma versao deflacionada por IPCA;
- comparar resultados em salario minimo e em reais constantes;
- separar compressao relativa ao salario minimo de queda real de poder de compra.

## 6. Robustez Da Classificacao Ciclica

A classificacao atual usa crescimento do PIB trimestral do SIDRA e marca 2015, 2016 e 2020 como crise.

Robustez recomendada:

- comparar com datacao CODACE;
- testar boom como crescimento acima de 1% versus outros limiares;
- testar especificacoes com e sem 2020;
- avaliar resultados com medias por fase e com series trimestrais completas.

## 7. Contrafactuais Do Modelo

Depois de melhorar o fit, o projeto deve reproduzir os contrafactuais do artigo original para o Brasil.

Possiveis exercicios:

- desligar mudancas no ciclo e manter apenas tendencia;
- desligar mudancas na participacao;
- manter transicoes fixas e alterar apenas salarios;
- decompor o papel de job loss, job finding, participacao e habilidades;
- comparar a contribuicao de recessoes brasileiras para a evolucao da desigualdade.

## 8. Fechamento Do Paper

O documento em `text/document.tex` ja contem a estrutura principal, mas ainda precisa de revisao final.

Pendencias:

- revisar abstract, introducao e contribuicao;
- consolidar a secao de dados;
- finalizar a secao do modelo;
- organizar tabelas de calibracao como no artigo original;
- incluir graficos finais apos a calibracao definitiva;
- revisar captions e labels;
- fechar conclusao distinguindo resultados robustos de resultados preliminares.

## Baselines E Versoes

A convencao atual de baselines e:

- `Brazil v1`: baseline inicial com transicoes agregadas e calibracao trabalhista preliminar;
- `Brazil v2`: baseline com transicoes por habilidade e bloco de participacao recalibrado;
- `Brazil v3`: candidato com calibracao salarial RAIS, atualmente representado pelo `trial 6`.

A recomendacao e congelar qualquer versao usada no texto em uma pasta propria dentro de:

```text
Propose/replication/HPV Brazil code/baselines/
```

Cada baseline deve conter:

- parametros usados;
- data da rodada;
- tempo de execucao;
- principais momentos de fit;
- figuras correspondentes;
- notas sobre limitacoes.

## Checklist De Proximas Etapas

- Congelar formalmente o candidato atual como baseline Brasil v3 preliminar.
- Fazer uma busca local nos parametros salariais ao redor do `trial 6`.
- Recalibrar participacao apos o ajuste salarial.
- Testar transicoes por habilidade mais proximas de uma medida continua.
- Produzir contrafactuais brasileiros analogos aos de HPV.
- Atualizar o documento com a versao final dos graficos.
- Compilar e revisar o PDF final.

