tests - converting
testZipped
	| compressed |
	
	compressed := 'hello' zipped.
	self assert: (compressed unzipped = 'hello').