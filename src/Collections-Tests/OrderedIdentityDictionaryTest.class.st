"
SUnit tests for OrderedIdentityDictionary
"
Class {
	#name : #OrderedIdentityDictionaryTest,
	#superclass : #OrderedDictionaryTest,
	#category : #Collections-Tests-Sequenceable
}

{ #category : #accessing }
OrderedIdentityDictionaryTest >> dictionaryClass [
	^ OrderedIdentityDictionary
]

{ #category : #testing }
OrderedIdentityDictionaryTest >> isTestingIdentityDictionary [
	^ true
]
