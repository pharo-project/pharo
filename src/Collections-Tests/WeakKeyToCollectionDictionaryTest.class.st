"
SUnit tests for weak key to collection dictionaries 
"
Class {
	#name : #WeakKeyToCollectionDictionaryTest,
	#superclass : #WeakIdentityKeyDictionaryTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #requirements }
WeakKeyToCollectionDictionaryTest >> classToBeTested [ 
 ^ WeakKeyToCollectionDictionary
]

{ #category : #'errors - those methods should be tested with collection keys' }
WeakKeyToCollectionDictionaryTest >> testKeysAndValuesRemove [
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
]

{ #category : #'errors - those methods should be tested with collection keys' }
WeakKeyToCollectionDictionaryTest >> testRemoveKey [	

"	| collection oldSize keyIn |
	collection := self nonEmptyDict .
	oldSize := collection size.
	keyIn := collection  keys anyOne.
	
	collection removeKey: keyIn .
	self assert: (collection  size = (oldSize - 1)).
	self should: [ (collection  at: keyIn )] raise: Error.
	
	self should: [collection removeKey: self keyNotInNonEmptyDict ] raise: Error"
]

{ #category : #'errors - those methods should be tested with collection keys' }
WeakKeyToCollectionDictionaryTest >> testRemoveKeyIfAbsent [
	
	"| collection oldSize keyIn value result |
	collection := self nonEmptyDict .
	oldSize := collection size.
	keyIn := collection  keys anyOne.
	value := collection at: keyIn .
	
	result := collection removeKey: keyIn ifAbsent: [888].
	
	self assert: result = value.
	self assert: (collection  size = (oldSize - 1)).
	self should: [ (collection  at: keyIn )] raise: Error.
	
	self assert: (collection removeKey: self keyNotInNonEmptyDict ifAbsent: [888] ) = 888."
]
