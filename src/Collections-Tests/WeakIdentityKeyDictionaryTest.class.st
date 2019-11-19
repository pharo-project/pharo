"
SUnit tests for weak identity key dictionaries
"
Class {
	#name : #WeakIdentityKeyDictionaryTest,
	#superclass : #WeakKeyDictionaryTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #requirements }
WeakIdentityKeyDictionaryTest >> canBeUnhealthy [
	^ false
]

{ #category : #coverage }
WeakIdentityKeyDictionaryTest >> classToBeTested [

	^ WeakIdentityKeyDictionary 
]

{ #category : #tests }
WeakIdentityKeyDictionaryTest >> testFinalizeValuesWhenLastChainContinuesAtFront [
	| objectWithHashModulo dictionary capacity a b c |
	objectWithHashModulo := [ :requestedHash :modulo | 
	| object |
	[ object := Object new.
	object hash \\ modulo = requestedHash ] whileFalse.
	object ].

	dictionary := self classToBeTested new.
	capacity := dictionary capacity.
	a := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: a put: 1.
	b := objectWithHashModulo value: capacity - 1 value: capacity.
	dictionary at: b put: 2.
	c := objectWithHashModulo value: capacity - 2 value: capacity.
	dictionary at: c put: 3.
	self assert: dictionary capacity equals: capacity.
	self assert: (dictionary array at: capacity - 1) key identicalTo: a.
	self assert: (dictionary array at: capacity) key identicalTo: b.
	self assert: (dictionary array at: 1) key identicalTo: c.
	a := nil.
	Smalltalk garbageCollect.
	dictionary finalizeValues.
	self assert: (dictionary includesKey: b).
	self assert: (dictionary includesKey: c).
	self assert: dictionary slowSize equals: 2
]

{ #category : #tests }
WeakIdentityKeyDictionaryTest >> testNoNils [
	| d |
	d := WeakIdentityKeyDictionary new
	at: 'hello' copy put: 'world';
		yourself.
	Smalltalk garbageCollect.
	self deny: (d keys includes: nil)
]
