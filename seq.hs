import System.Environment ( getArgs )   
import Text.Printf ( printf )
import Data.List.Split.Internals ( splitOn )

help :: IO ()
help = do
    printf "Uso: seq [OPÇÃO]... ÚLTIMO\n"
    printf " ou: seq [OPÇÃO]... PRIMEIRO ÚLTIMO\n"
    printf " ou: seq [OPÇÃO]... PRIMEIRO INCREMENTO ÚLTIMO\n"
    printf "Emite números de PRIMEIRO até ÚLTIMO, de INCREMENTO em INCREMENTO.\n\n"
    printf "Argumentos obrigatórios para opções longas também o são para opções curtas.\n"
    printf "  -f, --format=FORMATO     usa o FORMATO de ponto flutuante no estilo printf\n"
    printf "  -s, --separator=TEXTO    usa TEXTO para separar números (padrão: \\n)\n"
    printf "  -w, --equal-width        equaliza a largura preenchendo com zeros à esquerda\n"
    printf "      --help     mostra esta ajuda e sai\n"
    printf "      --version  informa a versão e sai\n\n"
    printf "Se são omitidos PRIMEIRO ou INCREMENTO, eles são tratados como 1. Isto é,\n"
    printf "um INCREMENTO omitido é tratado como 1 quando ÚLTIMO é menor que PRIMEIRO.\n"
    printf "A sequência de números termina quando a soma do número atual e o INCREMENTO\n"
    printf "se tornasse maior que ÚLTIMO.\n"
    printf "PRIMEIRO, INCREMENTO e ÚLTIMO são interpretados como valores em ponto\n"
    printf "flutuante.\n"
    printf "INCREMENTO geralmente é positivo se PRIMEIRO for menor que ÚLTIMO, e\n"
    printf "INCREMENTO geralmente é negativo se PRIMEIRO for maior que ÚLTIMO.\n"
    printf "INCREMENTO não deve ser 0; PRIMEIRO, INCREMENTO e ÚLTIMO não podem ser NaN.\n"
    printf "FORMATO deve estar apropriado para mostrar um argumento do tipo \"double\";\n"
    printf "por padrão, é %%.PRECf se PRIMEIRO, INCREMENTO e ÚLTIMO forem números\n"
    printf "decimais de ponto fixo com precisão máxima PREC, e é %%g caso contrário.\n\n"
    printf "Página de ajuda do GNU coreutils: <https://www.gnu.org/software/coreutils/>\n"
    printf "Relate erros de tradução do seq: <https://translationproject.org/team/pt_BR.html>\n"
    printf "Documentação completa em: <https://www.gnu.org/software/coreutils/seq>\n"
    printf "ou disponível localmente via: info \"(coreutils) seq invocation\"\n"

version :: IO ()
version = do 
    printf "seq (GNU coreutils) 8.30\n"
    printf "Copyright (C) 2018 Free Software Foundation, Inc.\n"
    printf "Licença GPLv3+: GNU GPL versão 3 ou posterior <https://gnu.org/licenses/gpl.html>\n"
    printf "Este é um software livre: você é livre para alterá-lo e redistribuí-lo.\n"
    printf "NÃO HÁ QUALQUER GARANTIA, na máxima extensão permitida em lei.\n\n"
    printf "Escrito por Ulrich Drepper.\n"

size :: Integer -> [Char] -> Integer
size acc x = if null (tail x) then acc else size (acc+1) (tail x) 

getSize :: [Char] -> Integer
getSize = size 1

getNoLeadFull :: [Char] -> [Char] -> [Char] -> [Char]
getNoLeadFull n acc1 acc2
    | null n = reverse acc2
    | head n == '0' = getNoLeadFull (tail n) (head n:acc1) acc2
    | otherwise = getNoLeadFull (tail n) "" (head n:(acc1++acc2))

getNoLead :: [Char] -> [Char]
getNoLead n = getNoLeadFull n "" ""

getPrecision :: [Char] -> Integer
getPrecision n 
    | null (tail (splitOn "." n)) || null (getNoLead (head (tail (splitOn "." n)))) = 0
    | otherwise = getSize (getNoLead (head (tail (splitOn "." n))))

myDiv :: Integer -> Integer -> Double
myDiv a b =  (fromIntegral a :: Double)/(fromIntegral b :: Double)

toValue :: [Char] -> Integer -> Integer
toValue n precision
    | head n == '-' = 
        -(toValue (tail n) precision)
    | null (tail (splitOn "." n)) || null (getNoLead (head (tail (splitOn "." n)))) = (10^precision)*(read (head (splitOn "." n)) :: Integer)
    | otherwise = 
        (10^precision)*(read (head (splitOn "." n)) :: Integer) + (read (getNoLead(head (tail (splitOn "." n)))) :: Integer)

isDouble :: [Char] -> Bool
isDouble st = not (null (tail st)) && (head st == '.' || isDouble (tail st))

printSeqInt :: [Char] -> [Char] -> Bool -> Integer -> Integer -> Integer -> Integer -> IO ()
printSeqInt f s w l inc r sz
    | r-(l+inc) > r-l ||  l > r = printf ""
    | w && f/="" = do
        printf "seq: o texto de formatação não pode ser especificado quando escrevendo textos de larguras iguais\n"
        printf "Tente \"seq --help\" para mais informações.\n"
    | f/="" && l+inc > r = do
        printf (f ++ "\n") (fromIntegral l :: Double)
        printSeqInt f s w (l+inc) inc r sz
    | f/="" = do
        printf (f ++ "%s") (fromIntegral l :: Double) s
        printSeqInt f s w (l+inc) inc r sz
    | w && l+inc > r = do
        printf ("%0" ++ show sz ++ "d\n") l
        printSeqInt f s w (l+inc) inc r sz
    | w = do 
        printf ("%0" ++ show sz ++ "d%s") l s
        printSeqInt f s w (l+inc) inc r sz
    | l+inc > r = do 
        printf "%d\n" l
        printSeqInt f s w (l+inc) inc r sz
    | otherwise = do
        printf "%d%s" l s
        printSeqInt f s w (l+inc) inc r sz

printSeqFakeDouble :: [Char] -> [Char] -> Bool -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
printSeqFakeDouble f s w l inc r sz precision
    | r-(l+inc) > r-l || l > r = printf ""
    | w && f/="" = do
        printf "seq: o texto de formatação não pode ser especificado quando escrevendo textos de larguras iguais\n"
        printf "Tente \"seq --help\" para mais informações.\n"
    | f/="" && l+inc > r = do
        printf (f ++ "\n") (myDiv l (10^precision))
        printSeqFakeDouble f s w (l+inc) inc r sz precision
    | f/="" = do
        printf (f ++ "%s") (myDiv l (10^precision)) s
        printSeqFakeDouble f s w (l+inc) inc r sz precision
    | w && l+inc > r = do
        printf ("%0" ++ show sz ++ "." ++ show precision ++ "f\n") (myDiv l (10^precision))
        printSeqFakeDouble f s w (l+inc) inc r sz precision
    | w = do 
        printf ("%0" ++ show sz ++ "."  ++  show precision ++  "f%s") (myDiv l (10^precision)) s
        printSeqFakeDouble f s w (l+inc) inc r sz precision
    | l+inc > r = do 
        printf ("%." ++  show precision ++ "f\n") (myDiv l (10^precision))
        printSeqFakeDouble f s w (l+inc) inc r sz precision
    | otherwise = do
        printf ("%." ++  show precision ++ "f%s") (myDiv l (10^precision)) s
        printSeqFakeDouble f s w (l+inc) inc r sz precision

printSeqHandler :: [Char] -> [Char] -> Bool -> [Char] -> [Char] -> [Char] -> IO ()
printSeqHandler f s w l inc r
    | isDouble l || isDouble inc || isDouble r = 
        printSeqFakeDouble f s w 
            (toValue l   (maximum [getPrecision l, getPrecision inc, getPrecision r])) 
            (toValue inc (maximum [getPrecision l, getPrecision inc, getPrecision r])) 
            (toValue r   (maximum [getPrecision l, getPrecision inc, getPrecision r])) 
            (getSize (head (splitOn "." r)) + maximum [getPrecision l, getPrecision inc, getPrecision r] + 1) 
            (maximum [getPrecision l, getPrecision inc, getPrecision r])
    | otherwise = 
        printSeqInt f s w (toValue l 0) (toValue inc 0) (toValue r 0) (getSize (head (splitOn "." r)))


printSeq :: [[Char]] -> [Char] -> [Char] -> Bool -> IO ()
printSeq (arg:args) f s w
    -- One arg
    | null args = 
        printSeqHandler f s w "1" "1" arg
        
    -- Two args
    | null (tail args) = 
        printSeqHandler f s w arg "1" (head args)
    
    -- Tree args
    | null (tail (tail args)) = 
        printSeqHandler f s w arg (head args) (head (tail args))
    
    | otherwise = do 
        printf "seq: operando extra “%s”\n" (head (tail (tail args)))
        printf "Tente \"seq --help\" para mais informações.\n"

argsHandlerFull :: [[Char]] -> [Char] -> [Char] -> Bool -> IO ()
argsHandlerFull (arg:args) f s w  
    | arg == "--help" = help
    | arg == "--version" = version
    | arg == "-w" = argsHandlerFull args f s True
    | arg == "--equal-width" = argsHandlerFull args f s True
    | arg == "-f" = argsHandlerFull (tail args) (head args) s w
    | take 9 arg == "--format=" = argsHandlerFull args (drop 9 arg) s w
    | arg == "-s" = argsHandlerFull (tail args) f (head args) w
    | take 12 arg == "--separator=" = argsHandlerFull args f (drop 12 arg) w
    | otherwise = printSeq (arg:args) f s w


argsHandler :: [[Char]] -> IO ()
argsHandler args = argsHandlerFull args "" "\n" False

main :: IO ()
main = do 
    args <- getArgs
    argsHandler args
