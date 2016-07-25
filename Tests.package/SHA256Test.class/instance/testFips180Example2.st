testing
testFips180Example2
	hash := SHA256 hashMessage: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	self assert: hash hex equals: '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1'