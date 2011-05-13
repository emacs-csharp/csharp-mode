_NETHOME=c:\.net3.5
_NETSDK20=c:\netsdk2.0\bin
_NETSDK=c:\netsdk3.5\bin

_CS_DBG_FLAGS=/debug:full /optimize-
_CS_DLL_FLAGS=/t:library  $(_CS_DBG_FLAGS)
_CS_EXE_FLAGS=/t:exe  $(_CS_DBG_FLAGS)

_CSC=$(_NETHOME)\csc.exe

!IFNDEF CONFIG
CONFIG=Release
!ENDIF


!IF "$(CONFIG)" == "Debug"
ADDL_DBG_DLLS=Ionic.CopyData.dll
ADDL_DBG_REFS=/R:ICSharpCode.NRefactory.dll /R:Ionic.CopyData.dll
ADDL_CSC_OPTIONS=/d:UseCopyData /d:CscompTrace
!Endif


default: CscompUtilities.dll


# debug: CscompUtilities.cs Ionic.CopyData.dll makefile
#         $(_CSC) /d:UseCopyData /d:CscompTrace /t:library /debug:full /optimize- /R:Ionic.CopyData.dll /out:CscompUtilities.dll CscompUtilities.cs


CscompUtilities.dll: makefile CscompUtilities.cs ICSharpCode.NRefactory.dll \
                     $(ADDL_DBG_DLLS)
        $(_CSC) /noconfig $(ADDL_CSC_OPTIONS) /t:library /debug:full /optimize- \
                /platform:anycpu \
                /R:System.Core.dll /R:System.dll /R:System.Windows.Forms.dll \
                $(ADDL_DBG_REFS) \
                /R:ICSharpCode.NRefactory.dll /out:$@ CscompUtilities.cs


TestDll.exe: makefile TestDll.cs CscompUtilities.dll
        $(_CSC) $(ADDL_CSC_OPTIONS) /t:exe /debug:full /optimize- \
                /R:CscompUtilities.dll /out:$@ TestDll.cs

