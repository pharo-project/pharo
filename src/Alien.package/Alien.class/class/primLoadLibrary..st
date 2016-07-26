primitives-library loading
primLoadLibrary: libraryName "<String> ^<Integer>"
	"Attempt to load the shared library/DLL named libraryName and
	 answer a handle which can be used in primFindSymbol:inLibrary:,
	 or fail if libraryName is invalid or libraryName cannot be found."
	<primitive: 'primLoadLibrary' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed