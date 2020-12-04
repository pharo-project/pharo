"
SUnit tests for OrderedIdentityDictionary
"
Class {
	#name : #OrderedIdentityDictionaryTest,
	#superclass : #OrderedDictionaryTest,
	#category : #'Collections-Sequenceable-Tests-Ordered'
}

{ #category : #accessing }
OrderedIdentityDictionaryTest >> dictionaryClass [
	^ OrderedIdentityDictionary
]

{ #category : #testing }
OrderedIdentityDictionaryTest >> isTestingIdentityDictionary [
	^ true
]
