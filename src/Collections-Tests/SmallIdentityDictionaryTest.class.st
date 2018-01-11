"
SUnit tests for small identity dictionaries
"
Class {
	#name : #SmallIdentityDictionaryTest,
	#superclass : #SmallDictionaryTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
SmallIdentityDictionaryTest >> canBeUnhealthy [
	^ false
]

{ #category : #requirements }
SmallIdentityDictionaryTest >> classToBeTested [ 
	^ SmallIdentityDictionary
]
