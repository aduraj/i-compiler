{-
Adrian Duraj, 18.09.2008, linux version

Compilation:
    ghc -c compiler.hs
    ghc -o ic compiler.o -package parsec
    You can remove compiler.hi and compiler.o
    
Using:
    ./ic [-o program_name] [-keep-asm-file] source_file_name
    
You need to have gcc and nasm installed on your system.
-}

module Main where

import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as PTok
import qualified Text.ParserCombinators.Parsec.Language as PLan
import qualified Text.ParserCombinators.Parsec.Char as PChar
import qualified Text.ParserCombinators.Parsec.Combinator as PComb
import qualified Text.ParserCombinators.Parsec.Expr as PExpr
import qualified Text.ParserCombinators.Parsec.Prim as PPrim
import qualified Text.ParserCombinators.Parsec.Error as PErr
import List(deleteBy)
import Maybe(fromMaybe)
import Char
import IO
import System
import List

-- Program syntax

type Identifier = String
type Contents = Integer
type Boolean = Bool

data AExpr = Const Contents | Ident Identifier |
             AExpr AExpr AOp AExpr deriving Show
data AOp = Aplus | Aminus | Atimes | Adiv | Amod deriving Show
data BExpr = BTrue | BFalse | BNot BExpr | BExpr BExpr BOp BExpr |
             RExpr AExpr ROp AExpr deriving Show
data BOp = BAnd | BOr deriving Show
data ROp = RLt | RLe | RGt | RGe | REq | RNe deriving Show
data Command = Skip | Abort | Identifier := AExpr | Command :. Command |
               If BExpr Command | IfElse BExpr Command Command |
               While BExpr Command | Read Identifier |
               Write AExpr deriving Show

-- Parser

parse :: String -> Either PErr.ParseError Command

parse = PPrim.parse (do
            PTok.whiteSpace lexer
            c <- program
            PComb.eof
            return c) "stdin" where
   langDef = PLan.LanguageDef {
      PLan.commentStart = "(*",
      PLan.commentEnd = "*)",
      PLan.commentLine = "#",
      PLan.nestedComments = True,
      PLan.identStart = PChar.letter,
      PLan.identLetter = PChar.alphaNum PPrim.<|> PChar.char '_',
      PLan.opStart = PLan.opLetter langDef,
      PLan.opLetter = PChar.oneOf (concat $ PLan.reservedOpNames langDef),
      PLan.reservedOpNames = [],
      PLan.reservedNames = ["skip", "abort", "if", "then", "else", "fi",
                            "while", "do", "done", "write", "read"],
      PLan.caseSensitive = False }
   lexer = PTok.makeTokenParser langDef

   program = do
      commands <- P.many1 (do {c <- semiCommand; PTok.semi lexer; return c} PPrim.<|> do {c <- noSemiCommand; return c})
      return $ case commands of
         [c] -> c
         cs -> (foldr1 (:.) cs)
   
   semiCommand = PComb.choice [
      PTok.reserved lexer "skip" >> return Skip,
      PTok.reserved lexer "abort" >> return Abort,
      do
         ident <- PTok.identifier lexer
         PTok.symbol lexer ":="
         aex <- aExpr
         return $ ident := aex,
      do
         PTok.reserved lexer "read"
         ident <- PTok.identifier lexer
         return $ Read ident,
      do
         PTok.reserved lexer "write"
         aex <- aExpr
         return $ Write aex]
   
   noSemiCommand = PComb.choice [
      do
         PTok.reserved lexer "if"
         bex <- bExpr
         PTok.reserved lexer "then"
         th <- program
         PComb.choice [
            PTok.reserved lexer "fi" >> return (If bex th),
            do
               PTok.reserved lexer "else"
               el <- program
               PTok.reserved lexer "fi"
               return (IfElse bex th el)],
      do
         PTok.reserved lexer "while"
         bex <- bExpr
         PTok.reserved lexer "do"
         pr <- program
         PTok.reserved lexer "done"
         return (While bex pr)]

   aExpr = PExpr.buildExpressionParser 
      [[op "*" PExpr.AssocLeft, op "div " PExpr.AssocLeft,
      op "mod " PExpr.AssocLeft], [op "+" PExpr.AssocLeft,
      op "-" PExpr.AssocLeft ]] primAExpr where
      op operation assoc = PExpr.Infix (do
         PTok.reservedOp lexer operation
         return (\ x y -> AExpr x (case operation of
            "*" -> Atimes
            "div " -> Adiv
            "mod " -> Amod
            "+" -> Aplus
            "-" -> Aminus) y)) assoc
      primAExpr = PComb.choice [       
         PTok.integer lexer >>= return . Const,
         PTok.identifier lexer >>= return . Ident,  
         PTok.parens lexer aExpr]

   bExpr = PExpr.buildExpressionParser 
      [[op "and" PExpr.AssocRight], [op "or" PExpr.AssocRight]] primBExpr where
      op operation assoc = PExpr.Infix (do
         PTok.reservedOp lexer operation
         return (\ x y -> BExpr x (case operation of
            "and" -> BAnd
            "or" -> BOr) y)) assoc
      primBExpr = PComb.choice [
         PTok.reserved lexer "true" >> return BTrue,
         PTok.reserved lexer "false" >> return BFalse,
         
         do
            PTok.reserved lexer "not"
            bex <- bSimpleExpr
            return $ BNot bex,

         P.try relationExpr, 
            
         PTok.parens lexer bExpr]
   
   bSimpleExpr  = do
      PComb.choice [
         PTok.reserved lexer "true" >> return BTrue,
         PTok.reserved lexer "false" >> return BFalse,
         do
            PTok.reserved lexer "not"
            bex <- bSimpleExpr
            return $ BNot bex,
         P.try relationExpr,
         PTok.parens lexer bExpr]

   relationExpr = do
         a1 <- aExpr
         rel <- PComb.choice [
            PChar.string "=",
            PPrim.try (PChar.string "<>"),
            PPrim.try (PChar.string "<="),
            PChar.string "<",
            PPrim.try (PChar.string ">="),
            PChar.string ">"]
         let op = case rel of
               "=" -> REq
               "<>" -> RNe
               "<=" -> RLe
               "<" -> RLt
               ">=" -> RGe
               ">" -> RGt
         PTok.whiteSpace lexer
         a2 <- aExpr
         return$ RExpr a1 op a2
         


--Assembler code generator

generate parsedCode = "segment .data\n\nprint_text db " ++ [chr 34] ++ "%d" ++ [chr 34] ++ ", 10, 13, 0\n"
                      ++ "scan_text db " ++ [chr 34] ++ "%d" ++ [chr 34] ++ ", 0\n"
                      ++ sectionData parsedCode [] 
                      ++ "\nsegment .text\n\nextern printf, scanf\n\nglobal main\nmain:\n\n"  
                      ++ sectionProgram parsedCode 0 ++ "exit:\n"

sectionData (c1 :. c2) xs = sectionData c1 xs ++ sectionData c2 (identCheck c1 xs)
sectionData (While b c) xs = sectionData c xs
sectionData (If b c) xs = sectionData c xs
sectionData (IfElse b c d) xs = sectionData c xs ++ sectionData d (identCheck c xs)

sectionData (Read x) xs 
   | elem x xs = ""
   | otherwise = "var_" ++ x ++ " dd 0\n"

sectionData (i := a) xs 
   | elem i xs = ""
   | otherwise = "var_" ++ i ++ " dd 0\n"

sectionData _ _ = ""


sectionProgram (c1 :. c2) x = sectionProgram c1 x ++ sectionProgram c2 (x + (counter c1))

sectionProgram Abort _ = "jmp exit\n"
sectionProgram Skip _ = "\n"

sectionProgram (i := a) _ = case a of
   Const n -> "mov dword [var_" ++ i ++ "], " ++ show n ++ "\n\n"
   AExpr a1 op a2 -> translateAExpr a ++ "mov dword [var_" ++ i ++ "], ebx\n"

sectionProgram (If b c) x = translateBExpr b x
                            ++ "mov ecx, 1\ncmp ebx, ecx\nje ICTOTHEN" ++ show x ++ "\n"
                            ++ "jmp ICTOFI" ++ show x ++"\n"
                            ++ "ICTOTHEN" ++ show x ++ ":\n"
                            ++ sectionProgram c (x + 1 + (counterB b))
                            ++ "ICTOFI" ++ show x ++ ":\n"

sectionProgram (IfElse b c d) x = translateBExpr b x
                            ++ "mov ecx, 1\ncmp ebx, ecx\nje ICTOTHEN" ++ show x ++ "\n" 
                            ++ "jmp ICTOELSE" ++ show x ++ "\n"
                            ++ "ICTOTHEN" ++ show x ++ ":\n"
                            ++ sectionProgram c (x + 2 + (counterB b))
                            ++ "jmp ICTOFI" ++ show (x + 1) ++ "\n"
                            ++ "ICTOELSE" ++ show x ++ ":\n"
                            ++ sectionProgram d (x + 2 + (counter c) + (counterB b))
                            ++ "ICTOFI" ++ show (x + 1) ++ ":\n"

sectionProgram (While b c) x = "ICTOWHILESTART" ++ show x ++ ":\n"
                               ++ translateBExpr b x
                               ++ "mov ecx, 1\ncmp ebx, ecx\nje ICTOWHILEBODY" ++ show x ++ "\n"
                               ++ "jmp ICTODONE" ++ show (x + 1) ++ "\n"
                               ++ "ICTOWHILEBODY" ++ show x ++ ":\n"
                               ++ sectionProgram c (x + 2 + (counterB b))
                               ++ "jmp ICTOWHILESTART" ++ show x ++ "\n"
                               ++ "ICTODONE" ++ show (x + 1) ++ ":\n"
   


sectionProgram (Read x) _ = "push var_" ++ x ++ "\npush dword scan_text\ncall scanf\nadd esp, 8\n\n"

sectionProgram (Write x) _ = case x of
   Const n -> "push " ++ show n ++ "\npush print_text\ncall printf\nadd esp, 8\n\n"
   Ident i -> "push dword [var_" ++ i ++ "]\npush print_text\ncall printf\nadd esp, 8\n\n"
   AExpr a1 op a2 -> translateAExpr x ++ "push ebx\npush print_text\ncall printf\nadd esp, 8\n\n"


translateAExpr (AExpr a1 op a2) = case op of
   Aplus -> translateAExpr a1
            ++ "push ebx\n"
            ++ translateAExpr a2
            ++ "push ebx\npop ebx\npop ecx\n"
            ++ "add ebx, ecx\n"
   Aminus -> translateAExpr a1
             ++ "push ebx\n"
             ++ translateAExpr a2
             ++ "push ebx\npop ecx\npop ebx\n"
             ++ "sub ebx, ecx\n"
   Atimes -> translateAExpr a1 
             ++ "push ebx\n"
             ++ translateAExpr a2
             ++ "push ebx\npop ebx\npop eax\n"
             ++ "mul ebx\nmov ebx, eax\n"
   Adiv -> translateAExpr a1 
           ++ "push ebx\n"
           ++ translateAExpr a2
           ++ "push ebx\nxor edx, edx\npop ebx\npop eax\n"
           ++ "div ebx\nmov ebx, eax\n"
   Amod -> translateAExpr a1 
           ++ "push ebx\n"
           ++ translateAExpr a2
           ++ "push ebx\nxor edx, edx\npop ebx\npop eax\n"
           ++ "div ebx\nmov ebx, edx\n"

translateAExpr (Const n) = "mov ebx, " ++ show n ++ "\n"

translateAExpr (Ident i) = "mov ebx, dword [var_" ++ i ++ "]\n"

translateBExpr (BExpr b1 op b2) x = case op of
   BAnd -> translateBExpr b1 x
           ++ "push ebx\n"
           ++ translateBExpr b2 (x + counterB b1)
           ++ "push ebx\npop ecx\npop ebx\n"
           ++ "and ebx, ecx\n"
   BOr -> translateBExpr b1 x
          ++ "push ebx\n"
          ++ translateBExpr b2 (x + counterB b1)
          ++ "push ebx\npop ecx\npop ebx\n"
          ++ "or ebx, ecx\n"

translateBExpr BFalse x = "mov ebx, 0\n"
translateBExpr BTrue x = "mov ebx, 1\n"

translateBExpr (RExpr a1 op a2) eip = case op of
   RLt -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "jl ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"
   RLe -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "jle ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"
   RGt -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "jg ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"
   RGe -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "jge ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"
   REq -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "je ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"
   RNe -> translateAExpr a1
          ++ "push ebx\n"
          ++ translateAExpr a2
          ++ "push ebx\n"
          ++ "pop ecx\npop ebx\ncmp ebx, ecx\n"
          ++ "jne ICRELATION" ++ show eip ++ "\n"
          ++ "mov ebx, 0\njmp ICRELATION" ++ show (eip + 1) ++ "\n"
          ++ "ICRELATION" ++ show eip ++ ":\nmov ebx, 1\nICRELATION" ++ show (eip + 1) ++":\n"

translateBExpr (BNot b) x = translateBExpr b x
                            ++ "not ebx\n"
                            ++ "mov ecx, 2\nadd ebx, ecx\n"


main :: IO()
main = do
   arguments <- getArgs
   argCheck arguments

identCheck (c1 :. c2) xs = identCheck c1 xs ++ identCheck c2 (identCheck c1 xs)
identCheck (If b c) xs = identCheck c xs
identCheck (IfElse b c d) xs = identCheck c xs ++ identCheck d (identCheck c xs)
identCheck (While b c) xs = identCheck c xs
identCheck (Read x) xs = (x:xs)
identCheck (i := a) xs = (i:xs)
identCheck _ xs = xs


counter (c1 :. c2) = (counter c1) + (counter c2)
counter (While b c) = 2 + (counter c) + (counterB b)
counter (IfElse b c d) = 2 + (counter c) + (counter d) + (counterB b)
counter (If b c) = 1 + (counter c) + (counterB b)
counter _ = 0


counterB (BExpr b1 op b2) = (counterB b1) + (counterB b2)
counterB (RExpr a1 op a2) = 2
counterB (BNot b) = counterB b
counterB _ = 0

argCheck xs
   | (head xs == "-o") && (head (tail (tail xs)) == "-keep-asm-file") && (length xs == 4) = do
      inputFile <- openFile (makeName (last xs)) ReadMode
      prog <- hGetContents inputFile 
      case Main.parse prog of
         Left err -> print err >> exitWith ExitSuccess
         Right ast -> writeFile (makeShortName (last xs) ++ ".asm") $ generate ast
      system ("nasm -f elf " ++ makeShortName (last xs) ++ ".asm")
      system ("gcc -o " ++ (head (tail xs)) ++ " " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".o")
      putStrLn "Compilation successful!"
      exitWith ExitSuccess
   | (head xs == "-o") && (length xs == 3) = do
      inputFile <- openFile (makeName (last xs)) ReadMode
      prog <- hGetContents inputFile
      case Main.parse prog of
         Left err -> print err >> exitWith ExitSuccess
         Right ast -> writeFile (makeShortName (last xs) ++ ".asm") $ generate ast
      system ("nasm -f elf " ++ makeShortName (last xs) ++ ".asm")
      system ("gcc -o " ++ (head (tail xs)) ++ " " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".asm")
      putStrLn "Compilation successful!"
      exitWith ExitSuccess
   | (head xs == "-keep-asm-file") && (length xs == 2)  = do
      inputFile <- openFile (makeName (last xs)) ReadMode
      prog <- hGetContents inputFile
      case Main.parse prog of
         Left err -> print err >> exitWith ExitSuccess
         Right ast -> writeFile (makeShortName (last xs) ++ ".asm") $ generate ast
      system ("nasm -f elf " ++ makeShortName (last xs) ++ ".asm")
      system ("gcc -o a.out " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".o")
      putStrLn "Compilation successful!"
      exitWith ExitSuccess
   | length xs == 1 = do
      inputFile <- openFile (makeName (last xs)) ReadMode
      prog <- hGetContents inputFile
      case Main.parse prog of
         Left err -> print err >> exitWith ExitSuccess
         Right ast -> writeFile (makeShortName (last xs) ++ ".asm") $ generate ast
      system ("nasm -f elf " ++ makeShortName (last xs) ++ ".asm")
      system ("gcc -o a.out " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".o")
      system ("rm " ++ makeShortName (last xs) ++ ".asm")
      putStrLn "Compilation successful!"
      exitWith ExitSuccess
   | otherwise = do
      putStrLn "Unknown command!"
      exitWith ExitSuccess


makeName arg
   | (head (reverse arg) == 'i') && (head (tail (reverse arg)) == '.') = arg
   | otherwise = arg ++ ".i"

makeShortName arg
   | (head (reverse arg) == 'i') && (head (tail (reverse arg)) == '.') = reverse (delete '.' (delete 'i' (reverse arg)))
   | otherwise = arg









