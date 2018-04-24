"
SUnit tests for identity dictionaries
"
Class {
	#name : #IdentityDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #'building suites' }
IdentityDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #requirements }
IdentityDictionaryTest >> canBeUnhealthy [
	^ false
]

{ #category : #coverage }
IdentityDictionaryTest >> classToBeTested [

	^ IdentityDictionary
]

{ #category : #'tests - identity' }
IdentityDictionaryTest >> testIdentity [
	| dict key |
	dict := self classToBeTested new.
	key := 'key'.
	dict at: key put: 2.5.
	 
	
	self assert: (dict includesKey: key).
	self deny: (dict includesKey: key copy).
	
"	dict at: 1 put: 'djdh'.
	dict at: 'sksl' put: 1.0.
	self deny: (dict includesKey: 1.0) .
	self assert: (dict includes: 1)"

]
