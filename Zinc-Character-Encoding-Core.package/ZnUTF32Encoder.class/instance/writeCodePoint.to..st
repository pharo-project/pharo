private
writeCodePoint: codePoint to: stream
	self isBigEndian 
		ifTrue: [ 
			stream 
				nextPut: (codePoint digitAt: 4); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 1) ]
		ifFalse: [ 
			stream 
				nextPut: (codePoint digitAt: 1); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 4) ]