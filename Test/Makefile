MONO=/usr/bin/mono
FSC=/usr/local/fsharp/bin/fsc.exe
COMPILER=${MONO} ${FSC}
FILES=Program.fs

target.exe: ${FILES}
        ${COMPILER} -o $@  ${FILES} -r FSharp.PowerPack.dll
