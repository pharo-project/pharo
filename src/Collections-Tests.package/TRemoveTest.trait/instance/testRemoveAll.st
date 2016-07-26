tests - remove
testRemoveAll
	"self debug: #testRemoveElementThatExists"
	| el aSubCollection collection res |
	collection := self nonEmptyWithoutEqualElements.
	el := collection anyOne.
	aSubCollection := collection copyWithout: el.
	res := collection removeAll: aSubCollection.
	self assert: collection size = 1.
	self nonEmptyWithoutEqualElements do: [ :each | self assert: each = el ]