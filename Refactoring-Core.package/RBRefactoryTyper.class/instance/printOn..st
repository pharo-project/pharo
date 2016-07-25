printing
printOn: aStream 
	aStream
		nextPutAll: class name;
		cr.
	class instanceVariableNames do: 
			[:each | 
			aStream
				tab;
				nextPutAll: each;
				tab;
				nextPut: $<.
			self printTypeFor: each on: aStream.
			aStream
				nextPut: $>;
				cr]