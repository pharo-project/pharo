tests - remove
testRemoveElementThatExists
	"self debug: #testRemoveElementThatExists"

	| el res |
	el := self nonEmptyWithoutEqualElements anyOne.
	res := self nonEmptyWithoutEqualElements remove: el.
	self assert: res == el