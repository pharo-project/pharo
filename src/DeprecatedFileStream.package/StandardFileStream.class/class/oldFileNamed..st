file creation
oldFileNamed: fileName
	"Open an existing file with the given name for reading and writing. If the name has no directory part, then the  default directory will be assumed. If the file does not exist, an exception is signaled. If the file exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	| fullName |
	fullName := self fullName: fileName.

	^(self isAFileNamed: fullName)
		ifTrue: [self new open: fullName forWrite: true]
		ifFalse: ["File does not exist..."
			(FileDoesNotExistException fileName: fullName) signal]