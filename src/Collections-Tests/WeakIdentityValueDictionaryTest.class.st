"
SUnit tests for WeakIdentityValueDictionaty
"
Class {
	#name : #WeakIdentityValueDictionaryTest,
	#superclass : #WeakValueDictionaryTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #requirements }
WeakIdentityValueDictionaryTest >> canBeUnhealthy [
	^ false
]

{ #category : #coverage }
WeakIdentityValueDictionaryTest >> classToBeTested [

	^ WeakIdentityValueDictionary 
]

{ #category : #tests }
WeakIdentityValueDictionaryTest >> testIdentity [
	| dict key |
	dict := self classToBeTested new.
	key := 'key'.
	dict at: key put: 2.5.
	 
	
	self assert: (dict includesKey: key).
	self deny: (dict includesKey: key copy).
]
