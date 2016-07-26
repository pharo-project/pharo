tests - remove
testRemoveElementReallyRemovesElement
	"self debug: #testRemoveElementReallyRemovesElement"
	| size |
	size := self nonEmptyWithoutEqualElements size.
	self nonEmptyWithoutEqualElements remove: self nonEmptyWithoutEqualElements anyOne.
	self assert: size - 1 = self nonEmptyWithoutEqualElements size