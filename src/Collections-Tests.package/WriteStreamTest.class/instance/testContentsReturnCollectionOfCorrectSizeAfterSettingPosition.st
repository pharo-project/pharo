testing
testContentsReturnCollectionOfCorrectSizeAfterSettingPosition
	| stream data finalPosition |
	stream := WriteStream on: String new.
	data := 'abcd'.
	finalPosition := 3.
	stream nextPutAll: data.
	stream position: finalPosition.
	self assert: stream contents equals: (data first: finalPosition)