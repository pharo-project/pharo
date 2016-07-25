private
write16BitWord: word toStream: stream
	self isBigEndian
		ifTrue: [ 
			stream
				nextPut: (word digitAt: 2);
				nextPut: (word digitAt: 1) ]
		ifFalse: [ 
			stream
				nextPut: (word digitAt: 1);
				nextPut: (word digitAt: 2) ]