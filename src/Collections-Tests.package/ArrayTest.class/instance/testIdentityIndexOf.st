tests - index access
testIdentityIndexOf
	"self debug: #testIdentityIndexOf"
	| collection element |
	element := self elementInCollectionOfFloat copy.
	self deny: self elementInCollectionOfFloat == element.
	collection := self collectionOfFloat copyWith: element.
	self assert: (collection identityIndexOf: element) = collection size