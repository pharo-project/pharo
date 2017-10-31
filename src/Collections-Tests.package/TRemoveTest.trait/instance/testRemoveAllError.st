tests - remove
testRemoveAllError
	"self debug: #testRemoveAllError"
	| el aSubCollection res |
	el := self elementNotIn.
	aSubCollection := self nonEmptyWithoutEqualElements copyWith: el.
	self 
		should: [ res := self nonEmptyWithoutEqualElements removeAll: aSubCollection ]
		raise: Error