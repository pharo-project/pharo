primitives-library loading
primFindSymbol: symbolName "<String> ^<Integer>"
	"The receiver is assumed to be a proxy whose first field contans the handle of a library
	 answered by primLoadLibrary.  Attempt to find symbolName in the library and answer
	 its address, or fail if the receiver is invalid or the symbol cannot be found."
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^errorCode == #'not found'
		ifTrue: [NotFoundError signal]
		ifFalse: [self primitiveFailed]