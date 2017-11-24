"
A trait for test purposes
"
Trait {
	#name : #TDictionaryAddingTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryAddingTest >> associationWithKeyAlreadyInToAdd [
	" return an association that will be used to add to nonEmptyDict (the key of this association is already included in nonEmptyDict)"
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryAddingTest >> associationWithKeyNotInToAdd [
	" return an association that will be used to add to nonEmptyDict (the key of this association is not included in nonEmptyDict)"
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryAddingTest >> nonEmptyDict [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryAddingTest >> test0FixtureDictionaryAddingTest [
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty.
	self associationWithKeyNotInToAdd.
	self deny: (self nonEmptyDict keys includes: self associationWithKeyNotInToAdd key).
	self associationWithKeyAlreadyInToAdd.
	self assert: (self nonEmptyDict keys includes: self associationWithKeyAlreadyInToAdd key)
]

{ #category : #'tests - adding' }
TDictionaryAddingTest >> testAddAll [
	
	| collectionToAdd collection result oldSize |
	collection := self nonEmptyDict .
	oldSize := collection size.
	collectionToAdd := self collectionClass new 
		add: self associationWithKeyAlreadyInToAdd ; 
		add: self associationWithKeyNotInToAdd ; 
		yourself.
	
	result := collection addAll: collectionToAdd .
	
	self assert: result = collectionToAdd .
	"  the association with the key already in should have replaced the oldest :"
	self assert: collection  size = (oldSize + 1).
	
	result associationsDo: [:assoc | self assert: (collection at:  (assoc key) ) = assoc value].
]

{ #category : #'tests - adding' }
TDictionaryAddingTest >> testAddWithKeyAlreadyIn [
	| dictionary result association oldSize |
	dictionary := self nonEmptyDict.
	oldSize := dictionary size.
	association := self associationWithKeyAlreadyInToAdd .
	result := dictionary add: association.
	
	self assert: result = association.
	self assert: (dictionary at: association key) = association value.
	self assert: dictionary size = oldSize .
]

{ #category : #'tests - adding' }
TDictionaryAddingTest >> testAddWithKeyNotIn [
	| dictionary result association oldSize |
	dictionary := self nonEmptyDict.
	oldSize := dictionary size.
	association := self associationWithKeyNotInToAdd.
	result := dictionary add: association.
	
	self assert: result = association.
	self assert: (dictionary at: association key) = association value.
	self assert: dictionary size = oldSize  + 1.
]

{ #category : #'tests - adding' }
TDictionaryAddingTest >> testDeclareFrom [
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
]
