converting
asArraySpecArrayedArgs
	^ String streamContents: [ :stream | |index|
		stream nextPutAll: '#( '.
		self printType: returnType on: stream.
		stream space.
		functionName ifNotNil: [
			stream 
				nextPutAll: functionName; 
				space ].
		stream nextPutAll: '( '.
		index := 1.
		arguments 
			do: [ :each | 
				stream nextPutAll: each second. "type name"
				each third 
					timesRepeat: [ stream nextPut: $* ].
				stream 
					space;
					nextPutAll: 'argsArray@', index printString;
					space.
				index := index + 1 ] 
			separatedBy: [ 
				stream nextPutAll: ', ' ].
		stream nextPutAll: '))' ]