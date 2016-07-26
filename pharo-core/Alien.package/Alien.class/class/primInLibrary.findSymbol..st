primitives-library loading
primInLibrary: libraryHandle "<Alien>" findSymbol: symbolName "<String> ^<Integer>"
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed