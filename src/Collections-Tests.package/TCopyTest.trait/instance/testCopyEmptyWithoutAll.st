tests - copy
testCopyEmptyWithoutAll
	"self debug: #testCopyEmptyWithoutAll"
	| res |
	res := self empty copyWithoutAll: self collectionWithElementsToRemove.
	self assert: res size = self empty size.
	self collectionWithElementsToRemove do: [ :each | self deny: (res includes: each) ]