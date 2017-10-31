tests - adding
testDeclareFrom
	| newDict v dictionary keyIn associationKeyNotIn |
	dictionary := self nonEmptyDict.
	keyIn := dictionary keys anyOne.
	associationKeyNotIn := self associationWithKeyNotInToAdd .
	newDict := self collectionClass new add: associationKeyNotIn; yourself.
	
	
	
	"if the key already exist, nothing changes"
	v := dictionary  at: keyIn.
	dictionary  declare: keyIn  from: newDict.
	self assert: (dictionary  at: keyIn ) = v.
	
	"if the key does not exist, then it gets removed from newDict and is added to the receiver"
	self nonEmptyDict declare: associationKeyNotIn key from: newDict.
	self assert: (dictionary  at: associationKeyNotIn key) = associationKeyNotIn value.
	self assert: (newDict size = 0)