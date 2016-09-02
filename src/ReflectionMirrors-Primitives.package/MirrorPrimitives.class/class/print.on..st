printing
print: anObject on: aStream
	| title |
	title := (self classOf: anObject) name.
	aStream 
		nextPutAll: ((title at: 1) isVowel ifTrue: ['an '] ifFalse: ['a ']).
	aStream nextPutAll: title