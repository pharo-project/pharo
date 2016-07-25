testing
testWikipediaExampleEmpty
	hash := SHA256 hashMessage: ''.
	self assert: hash hex equals: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'