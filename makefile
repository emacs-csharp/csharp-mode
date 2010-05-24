_NETHOME=c:\.net3.5
_NETSDK20=c:\netsdk2.0\bin
_NETSDK=c:\netsdk3.5\bin

_CS_DBG_FLAGS=/debug:full /optimize-
_CS_DLL_FLAGS=/t:library $(_CS_DBG_FLAGS)
_CS_EXE_FLAGS=/t:exe $(_CS_DBG_FLAGS)

_CSC=$(_NETHOME)\csc.exe

!IFNDEF CONFIG
CONFIG=Release
!ENDIF


!IF "$(CONFIG)" == "Debug"
ADDL_DEBUG_DLL=Ionic.CopyData.dll
ADDL_REF=/R:$(ADDL_DEBUG_DLL)
ADDL_CSC_OPTIONS=/d:UseCopyData
!Endif



default: CscompUtilities.dll

CscompUtilities.dll: makefile CscompUtilities.cs $(ADDL_DEBUG_DLL)
        $(_CSC) $(ADDL_CSC_OPTIONS) /t:library /debug:full /optimize- $(ADDL_REF) /out:$@ CscompUtilities.cs

