libraries
lookupOrNil: symbol  "<String>" inLibrary: libraryName "<String> ^<Alien>"
	"Answer the address of symbol in libraryName, or nil if it is not in the library.
	Exceptions will be raised for invalid libraries, symbol names, etc."
	^[self lookup: symbol inLibrary: libraryName ]
		on: NotFoundError
		do: [:ex| nil]