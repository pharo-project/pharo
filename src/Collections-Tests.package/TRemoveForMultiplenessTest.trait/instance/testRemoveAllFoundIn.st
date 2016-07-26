tests - remove
testRemoveAllFoundIn
	"self debug: #testRemoveElementThatExists"
	| el aSubCollection res |
	el := self nonEmptyWithoutEqualElements anyOne.
	aSubCollection := (self nonEmptyWithoutEqualElements copyWithout: el) copyWith: self elementNotIn.
	res := self nonEmptyWithoutEqualElements removeAllFoundIn: aSubCollection.
	self assert: self nonEmptyWithoutEqualElements size = 1.
	self nonEmptyWithoutEqualElements do: [ :each | self assert: each = el ]