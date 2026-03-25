"
SUnit tests for small identity dictionaries
"
Class {
	#name : 'SmallIdentityDictionaryTest',
	#superclass : 'SmallDictionaryTest',
	#category : 'Collections-Unordered-Tests-Dictionaries',
	#package : 'Collections-Unordered-Tests',
	#tag : 'Dictionaries'
}

{ #category : 'requirements' }
SmallIdentityDictionaryTest >> canBeUnhealthy [
	^ false
]

{ #category : 'coverage' }
SmallIdentityDictionaryTest >> classToBeTested [

	^ SmallIdentityDictionary
]

{ #category : 'tests - dictionary including' }
SmallIdentityDictionaryTest >> testIncludesAssociation [

	| dict key identityOrderedAssociations |
	key := 'testKey'.
	identityOrderedAssociations := Array with: (key := key copy) -> 'testValue'
		                               with: (key := key copy) -> 'testValue3'
		                               with: (key := key copy) -> 'testValue2'
		                               with: (key := key copy) -> 'testValue4'.

	dict := SmallIdentityDictionary new.
	identityOrderedAssociations do: [ :each | dict add: each ].
	self assert: dict size equals: 4.
	identityOrderedAssociations do: [ :each | self assert: (dict includesAssociation: each) ]
]
