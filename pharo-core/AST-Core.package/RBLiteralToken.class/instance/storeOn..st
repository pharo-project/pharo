printing
storeOn: aStream 
	value isSymbol 
		ifTrue: 
			[aStream nextPut: $#.
			((RBScanner isSelector: value) and: [value ~~ #'||']) 
				ifTrue: [aStream nextPutAll: value]
				ifFalse: [value asString printOn: aStream].
			^self].
	value class == Character 
		ifTrue: 
			[aStream
				nextPut: $$;
				nextPut: value.
			^self].
	value storeOn: aStream