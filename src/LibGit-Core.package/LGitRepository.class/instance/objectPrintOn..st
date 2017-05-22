printing
objectPrintOn: aStream
	aStream nextPutAll: self repositoryReference fullName.
	self isBare 
		ifTrue: [ aStream nextPutAll: ' [bare]' ]