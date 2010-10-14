Adrian Duraj, 18.09.2008

Compiler for I language.

Example in example.i file.
 
Generated .asm file (with option -keep-asm-file) - example.asm

LINUX VERSION:
Compilation:
    ghc -c compiler.hs
    ghc -o ic compiler.o -package parsec
    You can remove compiler.hi and compiler.o
    
Using:
    ./ic [-o program_name] [-keep-asm-file] source_file_name
    
You need to have gcc and nasm installed on your system.

WIN32 VERSION:
Compilation:
    ghc -c compiler.hs
    ghc -o ic compiler.o -package parsec
    You can remove compiler.hi, compiler.o and ic.exe.manifest 
    
Using:
    ic [-o program_name] [-keep-asm-file] source_file_name
    
You need to have gcc installed on your system and nasm.exe in your ic folder.