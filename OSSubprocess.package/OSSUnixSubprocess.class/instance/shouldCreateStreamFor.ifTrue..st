streams
shouldCreateStreamFor: aStream ifTrue: aBlock
	"Internal method. 
	If aStream was not specified explicitly by the user, but createMissingStandardStreams is true,
	then it evaluates aBlock"
	(aStream isNil and: [ createMissingStandardStreams ]) 
		ifTrue: [ aBlock value ]