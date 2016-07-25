testing
testWikipediaExample2
	hash := SHA256 hashMessage: 'The quick brown fox jumps over the lazy dog.'.
	self assert: hash hex equals: 'ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c'