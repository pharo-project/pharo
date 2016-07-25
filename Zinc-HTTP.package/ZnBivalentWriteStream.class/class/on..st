instance creation
on: writeStream
	"Wrap writeStream as a ZnBivalentWriteStream, unless it is already one"
	
	(writeStream isKindOf: self) ifTrue: [ ^ writeStream ].
	
	^ self basicNew
		on: writeStream;
		yourself