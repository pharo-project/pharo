tests - streaming
testNewStreamContentsReturnsCollectionOfCorrectSize
	| result item originalSize |
	originalSize := 10.
	item := self firstCollection first.
	result := self collectionClass
		new: originalSize
		streamContents: [ :stream | 
			1 to: originalSize + 1 do: [ :each | stream nextPut: item ].
			stream position: originalSize ].
	self assert: result size equals: originalSize