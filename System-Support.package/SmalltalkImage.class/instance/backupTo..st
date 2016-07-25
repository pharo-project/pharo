saving
backupTo: newNameWithoutSuffix
	"Create a new backup of this image. 
	Unlike #saveAs: do not transfer the default execution to the new image. 
	Results:
		true  when continuing in the new session
		false for the current session"
	SourceFiles changesFileStream ifNotNil:[
		self 
			closeSourceFiles; "so copying the changes file will always work"
			saveChangesInFileNamed: (self fullNameForChangesNamed: newNameWithoutSuffix)].
	
	^self backupImageInFileNamed: (self fullNameForImageNamed: newNameWithoutSuffix)