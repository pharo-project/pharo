source code management
getSourceFromFile
	"PLEASE Note: clients should always call #sourceCode"
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."

	^ [ SourceFiles sourceCodeAt: self sourcePointer ] on: Error do: [ '' ]