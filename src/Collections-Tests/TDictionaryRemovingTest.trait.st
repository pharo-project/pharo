"
A trait for test purposes
"
Trait {
	#name : #TDictionaryRemovingTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryRemovingTest >> emptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryRemovingTest >> keyNotInNonEmptyDict [
" return a key not included in nonEmptyDict"
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryRemovingTest >> newEmptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryRemovingTest >> nonEmptyDict [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryRemovingTest >> test0FixtureDictionaryRemovingTest [
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty.
	self keyNotInNonEmptyDict.
	self deny: (self nonEmptyDict keys includes: self keyNotInNonEmptyDict)
]

{ #category : #'tests - removing' }
TDictionaryRemovingTest >> testKeysAndValuesRemove [
	| oldSize collection keyIn |
	
	collection := self nonEmptyDict .
	oldSize := collection  size.
	keyIn := collection keys anyOne.
	
	collection  keysAndValuesRemove: [:key :value | key == self keyNotInNonEmptyDict ].
	self assert: (collection  size = (oldSize )).
	
	collection  keysAndValuesRemove: [:key :value | key == keyIn ].
	self assert: (collection  size = (oldSize - 1)).
	self should: [ collection at: keyIn  ] raise: Error.
]

{ #category : #'tests - removing' }
TDictionaryRemovingTest >> testRemove [

	self should: [self nonEmptyDict remove: nil] raise: Error.
	self should: [self nonEmptyDict remove: nil ifAbsent: ['What ever here']] raise: Error.
]

{ #category : #'tests - removing' }
TDictionaryRemovingTest >> testRemoveKey [
	
	| collection oldSize keyIn |
	collection := self nonEmptyDict .
	oldSize := collection size.
	keyIn := collection  keys anyOne.
	
	collection removeKey: keyIn .
	self assert: (collection  size = (oldSize - 1)).
	self should: [ (collection  at: keyIn )] raise: Error.
	
	self should: [collection removeKey: self keyNotInNonEmptyDict ] raise: Error
]

{ #category : #'tests - removing' }
TDictionaryRemovingTest >> testRemoveKeyIfAbsent [
	
	| collection oldSize keyIn value result |
	collection := self nonEmptyDict .
	oldSize := collection size.
	keyIn := collection  keys anyOne.
	value := collection at: keyIn .
	
	result := collection removeKey: keyIn ifAbsent: [888].
	
	self assert: result = value.
	self assert: (collection  size = (oldSize - 1)).
	self should: [ (collection  at: keyIn )] raise: Error.
	
	self assert: (collection removeKey: self keyNotInNonEmptyDict ifAbsent: [888] ) = 888.
]
