private
newSelectorString
	| stream keywords |
	stream := WriteStream on: String new.
	keywords := newSelector keywords.
	1 to: keywords size
		do: 
			[:i | 
			stream nextPutAll: (keywords at: i).
			i == keywords size 
				ifTrue: 
					[stream
						nextPut: $(;
						nextPutAll: initializer;
						nextPut: $)]
				ifFalse: 
					[stream
						nextPutAll: ' ``@arg';
						nextPutAll: i printString].
			stream nextPut: $ ].
	^stream contents