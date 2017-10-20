testing
testFips180Example1
	hash := SHA256 hashMessage: 'abc'.
	self assert: hash hex equals: 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'