########################################################################
#                                                                      #
# Program ID:  Makefile                                                #
#                                                                      #
# Description: Makefile for Muster Optim                               #
#                                                                      #
# Author :     Kyriacou Stylianos 17/Jul/2017                          #
#                                                                      #
########################################################################

LIB_NAME = Program.exe
DLL_NAME = EAlib.dll

# Linux Compiler
# -------------- 
GPP = fsharpc

SYS_LIBS_LINUX   = ./lib/MathNet.Numerics.dll

#
# Objects
# -------
OBJECTS         =   EA.fs 
#OBJECTS         =   EA.fs EA2.fs 

EXE_OBJ		= Program.fs

 
default:
	@echo 
	@echo "    "     linexe  ""   - to create .exe for linux
	@echo "    "     clean  ""   - cleans all .dlls and $(LIB_NAME)
	
#
# LINUX
# -----
linexe: $(OBJECTS) $(JOBJECT)
	@echo ------------EA Core Compiled $(OBJECTS)
	@$(GPP) -r $(SYS_LIBS_LINUX) -a $(OBJECTS) -o $(DLL_NAME)
	@echo ------------Linking Application $(LIB_NAME)
	@$(GPP) -r $(SYS_LIBS_LINUX) -r $(DLL_NAME) $(EXE_OBJ) -o $(LIB_NAME)
	@cp  $(SYS_LIBS_LINUX) .

#
# Cleaning
# --------
clean:
	@echo Cleaning core
	@rm  *.dll  $(LIB_NAME)


