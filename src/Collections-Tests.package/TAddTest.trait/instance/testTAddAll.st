tests - adding
testTAddAll
	| added collection toBeAdded |
	collection := self collectionWithElement .
	toBeAdded := self otherCollection .
	added := collection addAll: toBeAdded .
	self assert: added == toBeAdded .	"test for identiy because #addAll: has not reason to copy its parameter."
	self assert: (collection includesAll: toBeAdded )