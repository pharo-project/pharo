file creation
readOnlyFileNamed: fileName 
	"Open an existing file with the given name for reading."

	| fullName f |
	fullName := self fullName: fileName.
	f := self new open: fullName forWrite: false.
	^ f ifNil: ["File does not exist..."
			((FileDoesNotExistException fileName: fullName) readOnly: true) signal].

	"StandardFileStream readOnlyFileNamed: 'kjsd.txt' "