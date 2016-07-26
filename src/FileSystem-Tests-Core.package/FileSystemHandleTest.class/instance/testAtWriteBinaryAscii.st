tests
testAtWriteBinaryAscii
	handle
		at: 1
		write: #[32]
		startingAt: 1
		count: 1.
	handle
		at: 1
		write: (String with: Character space)
		startingAt: 1
		count: 1