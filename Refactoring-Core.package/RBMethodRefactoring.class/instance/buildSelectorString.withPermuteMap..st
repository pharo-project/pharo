private
buildSelectorString: aSelector withPermuteMap: anIntegerCollection 
	| stream keywords |
	aSelector numArgs == 0 ifTrue: [^aSelector asString].
	stream := WriteStream on: String new.
	keywords := aSelector keywords.
	keywords with: anIntegerCollection
		do: 
			[:each :i | 
			stream
				nextPutAll: each;
				nextPutAll: ' ``@arg';
				nextPutAll: i printString;
				nextPut: $ ].
	^stream contents