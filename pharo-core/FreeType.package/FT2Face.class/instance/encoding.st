accessing
encoding
	encoding ifNil: [ self getCharMap ].
	^encoding