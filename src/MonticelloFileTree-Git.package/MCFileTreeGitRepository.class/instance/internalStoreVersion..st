storing
internalStoreVersion: aVersion
	"Do a filetree store followed by a git add command."

	"Do not commit yet. Commit will only happens after checking the dependencies."
	
	"Writer is metadata dependent."

	| packageDirectoryString |
	self readonly
		ifTrue:
			[ ^ self
				error:
					'The gitfiletree repository: ' , self description printString
						, ' was created read only.' ].
	(self hasMetadata
		ifTrue: [ MCFileTreeWriter ]
		ifFalse: [ MCFileTreeGitWriter ]) fileOut: aVersion on: self.
	packageDirectoryString := (self class parseName: aVersion info name)
		first , self packageExtension.
	aVersion dependencies notEmpty
		ifTrue:
			[ self
				writeGitFileTreeProperties:
					(self fileUtils
						directoryFromPath: packageDirectoryString
						relativeTo: directory) ].
	self
		gitCommand:
			{'add'.
			packageDirectoryString}
		in: directory