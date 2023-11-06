Extension { #name : 'MetacelloPlatform' }

{ #category : '*Deprecated12' }
MetacelloPlatform >> authorName: aString [

	self deprecated: 'Use `Author fullName: aString` instead.' transformWith: '`@rcv authorName: `@arg' -> 'Author fullName: `@arg'.
	Author fullName: aString
]
