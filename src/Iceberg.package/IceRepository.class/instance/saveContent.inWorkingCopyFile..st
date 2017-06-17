actions
saveContent: aString inWorkingCopyFile: aRelativePathString
	| outputFile |
	outputFile := (self backend repositoryDirectory fullName , '/' , aRelativePathString) asFileReference.
	outputFile writeStreamDo: 
		[ :stream |
		stream nextPutAll: aString ].
	self addFilesToIndex: { aRelativePathString }.
	^ outputFile