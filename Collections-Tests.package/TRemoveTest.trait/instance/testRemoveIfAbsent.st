tests - remove
testRemoveIfAbsent
	"self debug: #testRemoveElementThatExists"

	| el res |
	el := self elementNotIn.
	res := self nonEmptyWithoutEqualElements remove: el ifAbsent: [ 33 ].
	self assert: res = 33