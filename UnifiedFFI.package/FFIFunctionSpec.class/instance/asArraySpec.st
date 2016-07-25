converting
asArraySpec
	^ String streamContents: [ :stream | 
		stream nextPutAll: '#( '.
		self printType: returnType on: stream.
		stream space.
		functionName ifNotNil: [
			stream 
				nextPutAll: functionName; 
				space ].
		stream nextPutAll: '( '.
		arguments 
			do: [ :each | 
				stream nextPutAll: each second. "type name"
				each third 
					timesRepeat: [ stream nextPut: $* ].
				stream 
					space;
					nextPutAll: each first;
					space ] 
			separatedBy: [ stream nextPutAll: ', ' ].
		stream nextPutAll: '))' ]