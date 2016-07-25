file creation
newFileNamed: fileName
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, ask the user what to do."

	| fullName |
	fullName := self fullName: fileName.

	^(self isAFileNamed: fullName)
		ifTrue: ["file already exists:"
			(FileExistsException fileName: fullName fileClass: self) signal]
		ifFalse: [self new open: fullName forWrite: true]

