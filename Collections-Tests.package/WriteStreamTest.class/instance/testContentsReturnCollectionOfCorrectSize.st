testing
testContentsReturnCollectionOfCorrectSize
	| stream data |
	stream := WriteStream on: String new.
	data := 'abcd'.
	stream nextPutAll: data.
	self assert: stream contents equals: data