errors - those methods should be tested with collection keys
testKeysAndValuesRemove
	" should be tested with collection keys"
	
	"| oldSize collection keyIn |
	
	
	collection := self nonEmptyDict .
	oldSize := collection  size.
	keyIn := collection keys anyOne.
	
	collection  keysAndValuesRemove: [:key :value | key == self keyNotInNonEmptyDict ].
	self assert: (collection  size = (oldSize )).
	
	collection  keysAndValuesRemove: [:key :value | key == keyIn ].
	self assert: (collection  size = (oldSize - 1)).
	self should: [ collection at: keyIn  ] raise: Error."