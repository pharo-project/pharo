primitives-Windows-VM-specific
primInIOProcessEventsFlagAddress "^<Integer>"
	"Answer the address of the Windows-VM-specific int inIoProcessEvents flag.
	 See platforms/win32/vm/sqWin32Window.c
	 and platforms/Cross/plugins/IA32ABI/ia32abicc.c."
	<primitive: 'primInIOProcessEventsFlagAddress' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed