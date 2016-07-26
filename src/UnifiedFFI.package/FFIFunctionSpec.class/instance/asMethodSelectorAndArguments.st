converting
asMethodSelectorAndArguments
	^ String streamContents: [ :stream | 
		stream nextPutAll: functionName.
		arguments ifNotEmpty: [
			arguments size = 1 ifFalse: [
				stream
					nextPut: $_;
					nextPutAll: arguments first first ].
			stream
				nextPut: $:; 
				space;
				nextPutAll: arguments first first.
				
			arguments allButFirst 
				do: [:each | 
					stream 
						space;
						nextPutAll: each first; 
						nextPut: $:;
						space;
						nextPutAll: each first ]]]