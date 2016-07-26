converting
convertChar: aCharacter 

	self checkBufferSpace: 8.

	bs reset.
	encoder nextPut: aCharacter toStream: bs.
	bs nextPut: 0.
	^ buffer