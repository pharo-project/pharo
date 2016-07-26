saving
saveAs: newNameWithoutSuffix
	"Results:
		true  when continuing in the new session
		false for the current session "

	newNameWithoutSuffix ifNil: [ ^ self ].
	SourceFiles changesFileStream ifNotNil: [ 
		self closeSourceFiles.
		self saveChangesInFileNamed: (self fullNameForChangesNamed: newNameWithoutSuffix)	
		"so copying the changes file will always work" ].

	^ self saveImageInFileNamed: (self fullNameForImageNamed: newNameWithoutSuffix)