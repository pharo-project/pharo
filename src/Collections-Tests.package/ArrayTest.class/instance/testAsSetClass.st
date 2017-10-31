tests
testAsSetClass
	| |
	"test with a collection without equal elements :"
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set.
	