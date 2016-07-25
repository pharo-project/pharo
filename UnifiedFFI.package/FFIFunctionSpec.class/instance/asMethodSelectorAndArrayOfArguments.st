converting
asMethodSelectorAndArrayOfArguments
	^ String streamContents: [ :stream | 
		stream
			nextPutAll: functionName;
			nextPutAll: ': argsArray';
			cr;
			nextPut: $".
		arguments 
			do: [ :each | stream nextPutAll: each first ] 
			separatedBy: [ stream nextPutAll: ', ' ].
		stream nextPut: $" ]