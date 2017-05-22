printing
printOn: aStream
	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [ 'an ' ] ifFalse: [ 'a ' ]);
		nextPutAll: title.
		
	(self shouldContinuePrintingOn: aStream) ifFalse: [ ^ self ].
	
	aStream nextPutAll: ' ('.
	self objectPrintOn: aStream.
	aStream nextPut: $)