module versions

data Aco = Generico | Antigo | Estrutural | EstruturalEscondido
data Next = Ligada | Desligada
data Arquivo = Versionado | NaoVersionado | VersionadoInterno
data Secao = DefaultRetangular | Outras

total converteNext: Aco -> Aco
converteNext Generico = Antigo
converteNext EstruturalEscondido = Estrutural
converteNext Antigo = Antigo
converteNext Estrutural = Estrutural

total converte: Aco -> Aco
converte Generico = Generico
converte EstruturalEscondido = EstruturalEscondido
converte Antigo = Generico
converte Estrutural = EstruturalEscondido

total cfgAntiga : Next -> Aco -> (Aco, Aco)
cfgAntiga Ligada a = (converteNext a, Estrutural)
cfgAntiga Desligada a = (a, EstruturalEscondido)

total cfg : Next -> (Aco, Aco) -> (Aco, Aco)
cfg Ligada (a, b) = (converteNext a, converteNext b)
cfg Desligada (a, b) = (converte a, converte b)

total elementos : Next -> Aco -> Aco
elementos Ligada Generico = Antigo
elementos Ligada Antigo = Antigo
elementos Ligada Estrutural = Estrutural
elementos Ligada EstruturalEscondido = Estrutural
elementos Desligada Generico = Generico
elementos Desligada Antigo = Antigo
elementos Desligada Estrutural = EstruturalEscondido
elementos Desligada EstruturalEscondido = EstruturalEscondido

total tratarSecaoElemento : Next -> Secao -> Secao
tratarSecaoElemento Ligada DefaultRetangular = DefaultRetangular
tratarSecaoElemento Ligada Outras = Outras
tratarSecaoElemento Desligada DefaultRetangular = DefaultRetangular
tratarSecaoElemento Desligada Outras = DefaultRetangular
