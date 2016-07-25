private
buildSelectorString: aSelector 
	| stream keywords |
	aSelector numArgs = 0 ifTrue: [^aSelector].
	stream := String new writeStream.
	keywords := aSelector keywords.
	1 to: keywords size
		do: 
			[:i | 
			stream
				nextPutAll: (keywords at: i);
				nextPutAll: ' ``@arg';
				nextPutAll: i printString;
				nextPut: $ ].
	^stream contents