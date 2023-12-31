# Máquina de Baixo Nível - Projeto de Programação Funcional e Lógica

## Introdução
Neste projeto realizado no âmbito da UC Programação Funcional e em Lógica, implementamos uma máquina de baixo nível para execução de programas representados por uma lista de instruções específicas. O objetivo é simular a execução de programas escritos nesse conjunto particular de instruções.

### Descrição da Máquina
A máquina consiste em configurações representadas por tuplas compostas por lista de instruções (Code), pilha de avaliação (Stack) e estado de armazenamento (State).

#### Elementos da Configuração (Code, Stack, State)
- `Code`: Lista de instruções a serem executadas.
- `Stack`: Pilha para armazenamento de valores inteiros e booleanos.
- `State`: Representação do estado de armazenamento, associando chaves (strings) a valores inteiros.

### Instruções Disponíveis
A máquina possui um conjunto específico de instruções, como operações aritméticas, booleanas e operações de controle de fluxo.

#### Exemplos de Instruções
- **Operações Aritméticas**: `Push n`, `Add`, `Mult`, `Sub`
- **Operações Booleanas**: `Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`
- **Operações Condicionais**: `Branch c1 c2`, `Loop c1 c2`
- **Outras Operações**: `Fetch x`, `Store x`, `Noop`

### Definição de Datas e Types
 - **Data Value**:
data Value = IntValue Integer | BoolValue Bool 
deriving Show

O data Value é utilizado para representar os valores que podem estar na Stack durante a execução do programa. 
 - **Type Stack**:
type Stack = [Value]

O type Stack representa o tipo da Stack de avaliação da máquina.
 - **Type State**:
type State = [(String, Value)]

O type State representa o tipo do estado da máquina. O estado associa variáveis (representadas por strings) a valores inteiros. Este tipo é utilizado para armazenar o estado da máquina durante a execução do programa.
 - **Data Aexp**:
data Aexp = IntValue' Integer | Variable' String | Add' Aexp Aexp | Sub' Aexp Aexp | Mul' Aexp Aexp 
deriving (Show) 

O data Aexp representa expressões aritméticas na linguagem. Pode ser uma constante inteira (IntValue'), uma variável (Variable'), ou uma combinação de adição (Add'), subtração (Sub') e multiplicação (Mul') de outras expressões aritméticas.
 - **Data Bexp**:
data Bexp = BoolValue' Bool | Eqb' Bexp Bexp | And' Bexp Bexp | Not' Bexp | Le' Aexp Aexp | Eqa' Aexp Aexp 
deriving (Show) 

O data Bexp representa expressões booleanas. Pode ser um valor booleano (BoolValue'), uma comparação de igualdade (Eqb'), uma operação lógica "E" (And'), a negação de uma expressão booleana (Not'), ou comparações "menor ou igual" (Le') e "igual" (Eqa') entre expressões aritméticas.
 - **Data Stm**:
data Stm = While Bexp [Stm] | Attrib String Aexp | If Bexp [Stm] [Stm] | Aexp' Aexp 
deriving (Show) 

O data Stm representa declarações (statements) na linguagem. Pode ser um loop While, uma atribuição (Attrib), uma estrutura condicional If, ou uma expressão aritmética (Aexp').
 - **Data Token**:
data Token = IntTok Integer | VarTok String | AddTok | SubTok | MulTok | OpenTok | CloseTok | BoolTok Bool | EqaTok | AndTok | NotTok | LeTok | WhileTok | AttribTok | IfTok | ThenTok | ElseTok | EqbTok | DoTok 
deriving (Show, Eq)

O data Token é utilizado para representar os diferentes tipos de tokens que podem ser gerados durante a análise do código. Cada token possui uma forma específica de representar informações.
 - **Type Program**:
type Program = [Stm] 

O type Program é uma lista de declarações (Stm), representando um programa na linguagem.
### Compilação e Execução
 - **compA**:
A função compA compila uma expressão aritmética (Aexp) para código de máquina (Code). Utiliza a abordagem de compilação recursiva para traduzir a expressão em instruções da máquina de baixo nível.
Vamos considerar a expressão aritmética 2 * (x + 3).

Agora, utilizando a função compA, podemos compilar essa expressão para código de máquina:

compA (Mul' (IntValue' 2) (Add' (Variable' "x") (IntValue' 3))) 

Resultado: [Push 3,Fetch "x",Add,Push 2,Mult]
 - **compB**:
A função compB compila uma expressão booleana (Bexp) para código de máquina (Code). Assim como compA, usa a abordagem de compilação recursiva para converter a expressão em instruções da máquina.
Vamos considerar um exemplo simples:

compB (And' (BoolValue' True) (Not' (BoolValue' False))) 

Resultado: [Fals,Neg,Tru,And]
 - **compStm**:
A função compStm compila uma declaração (Stm) para código de máquina (Code). Usa a abordagem de compilação recursiva para converter diferentes tipos de declarações em instruções da máquina.
Consideremos a declaração x = 5. Vamos compilar esta declaração utilizando a função compStm:

compStm (Attrib "x" (IntValue' 5)) 

Resultado: [Push 5,Store "x"]
 - **compile**:	
A função compile recebe um programa (Program) e converte-o para código de máquina (Code). Utiliza a abordagem de compilação recursiva para processar cada declaração no programa.
Agora, vamos criar um programa simples que consiste em duas declarações: uma atribuição e uma expressão aritmética.

compile [Attrib "x" (IntValue' 5), Aexp' (Add' (Variable' "x") (IntValue' 3))] 

Resultado: [Push 5,Store "x",Push 3,Fetch "x",Add]
 - **lexer**:
A função lexer recebe uma string e converte-a numa lista de tokens (Token). Esta função é responsável pela análise léxica do código fonte.
Considere o seguinte código fonte:

lexer "x := 5; y := x + 3" 

Resultado: [VarTok "x", AttribTok, IntTok 5, VarTok "y", AttribTok, VarTok "x", AddTok, IntTok 3]
 - **parseProg**:
A função parseProg é responsável por analisar uma lista de tokens e construir a representação interna do programa. O resultado é uma lista de instruções (Program) representando o programa Haskell.
Vamos criar um exemplo de código fonte que contém uma atribuição e uma expressão aritmética, e em seguida, utilizaremos a função parseProg para converter isso num programa:

parseProg [VarTok "x", AttribTok, IntTok 5, VarTok "y", AttribTok, VarTok "x", AddTok, IntTok 3] 

Resultado: [Attrib "x" (IntValue' 5), Attrib “y” (Add' (Variable' "x") (IntValue' 3))] 

Neste exemplo, o código fonte é analisado para criar uma lista de tokens. A função parseProg é então aplicada para gerar o programa correspondente, bem como a lista de tokens restantes.
 - **parse**:
A função parse é responsável por converter um código fonte numa representação interna do programa. Esta função utiliza as funções auxiliares lexer e parseProg. O processo é dividido em duas etapas principais:

1. Lexer (lexer): Converte o código fonte numa lista de tokens, representando os elementos básicos do código, como operadores, variáveis, números, etc.

2. Parser (parseProg): Converte a lista de tokens gerada pelo lexer numa lista de statements, utilizando a gramática da linguagem definida pelas regras da função parseProg.

A função parse simplifica a chamada dessas duas etapas, fornecendo diretamente o programa resultante.

Exemplo:

parse "x := 5; y := x + 3"

Resultado: [Attrib "x" (IntValue' 5), Attrib "y" (Add' (Variable' "x") (IntValue' 3))] 
## Testes
Usamos os seguintes testes para verificar o funcionamento do código

- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
- testAssembler [Push (-20),Push (-21), Le] == ("True","")
- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
- testAssembler [Push 1,Push 2,And]
  
You should get an exception with the string: "Run-time error"
- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
  
You should get an exception with the string: "Run-time error"
  
- testParser "x := 5; x := x - 1;" == ("","x=4")
- testParser "x := 0 - 2;" == ("","x=-2")
- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

## Autores
- Nuno Rodrigo Moreira Silva
- Tiago da Silva Azevedo

## Data
Dezembro de 2023
