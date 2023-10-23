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
