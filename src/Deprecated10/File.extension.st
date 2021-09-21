Extension { #name : #File }

{ #category : #'*Deprecated10' }
File class >> openAndTruncateForWriteFileNamed: aName [

	self deprecated: 'Use FileReference API for example ''filename'' asFileReference ensureDelete'.
	^ (self named: aName)
		delete;
		openForWrite
]
