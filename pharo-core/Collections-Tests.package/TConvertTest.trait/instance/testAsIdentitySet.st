tests - converting
testAsIdentitySet
	"test with a collection without equal elements :"
	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet.
