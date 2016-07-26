tests - remove
testRemoveAllError
	"self debug: #testRemoveElementThatExists"
	| el aSubCollection |
	el := self elementNotIn.
	aSubCollection := self nonEmptyWithoutEqualElements copyWith: el.
	self 
		should: [ | res | res := self nonEmptyWithoutEqualElements removeAll: aSubCollection ]
		raise: Error