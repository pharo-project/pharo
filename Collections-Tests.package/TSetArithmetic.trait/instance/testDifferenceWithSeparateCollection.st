tests - set arithmetic
testDifferenceWithSeparateCollection
	"Answer the set theoretic difference of two collections."
	"self debug: #testDifferenceWithSeparateCollection"
	| res separateCol |
	
	separateCol := self collectionClass with: self anotherElementOrAssociationNotIn.
	res := self collectionWithoutEqualElements difference: separateCol.
	
	self deny: (res includes: self anotherElementOrAssociationNotIn).
	self assert: res size equals: self collectionWithoutEqualElements size.
	
	self collectionWithoutEqualElements do: [ :each|
		self assert: (res includes: each)].
	
	res := separateCol difference: self collection.
	self deny: (res includes: self collection anyOne).
	self assert: res equals: separateCol