tests
testContents
	| ref contents |
	
	contents := '12345 abcdf!'.
	ref := filesystem * 'file'.
	
	ref writeStreamDo: [ :stream |
		stream nextPutAll: contents ].
	
	self assert: ref contents asString equals: contents