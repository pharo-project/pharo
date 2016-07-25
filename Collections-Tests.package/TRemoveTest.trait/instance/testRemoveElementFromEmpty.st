tests - remove
testRemoveElementFromEmpty
	"self debug: #testRemoveElementFromEmpty"
	self 
		should: [ self empty remove: self nonEmptyWithoutEqualElements anyOne ]
		raise: Error