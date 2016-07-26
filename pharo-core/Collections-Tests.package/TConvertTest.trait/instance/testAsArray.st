tests - converting
testAsArray
	"self debug: #testAsArray3"
	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: Array