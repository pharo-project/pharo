tests - adding
testTAdd
	| added collection |
	collection :=self otherCollection .
	added := collection add: self element.
	
	self assert: added == self element.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection includes: self element)	.
	self assert: (self collectionWithElement includes: self element).
	
	