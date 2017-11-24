"
SUnit tests for WeakIdentityValueDictionaty
"
Class {
	#name : #WeakIdentityValueDictionaryTests,
	#superclass : #WeakValueDictionaryTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #requirements }
WeakIdentityValueDictionaryTests >> canBeUnhealthy [
	^ false
]

{ #category : #requirements }
WeakIdentityValueDictionaryTests >> classToBeTested [

	^WeakIdentityValueDictionary 
]

{ #category : #tests }
WeakIdentityValueDictionaryTests >> testIdentity [
	| dict key |
	dict := self classToBeTested new.
	key := 'key'.
	dict at: key put: 2.5.
	 
	
	self assert: (dict includesKey: key).
	self deny: (dict includesKey: key copy).
]
