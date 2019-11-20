Class {
	#name : #WeakIdentitySetTest,
	#superclass : #WeakSetTest,
	#category : #'Collections-Tests-Weak'
}

{ #category : #requirements }
WeakIdentitySetTest >> classToBeTested [

	^ WeakIdentitySet
]

{ #category : #tests }
WeakIdentitySetTest >> testAddEqualElements [
	| ws o2 o3 |
	o2 := 1 / 2.
	o3 := '123' copy.
	ws := self classToBeTested new.
	ws add: o2.
	ws add: o3.
	self assert: ws size equals: 2.
	self assert: (ws includes: o2).
	self assert: (ws includes: o3).

	"inclusion test does use identity, not equality"
	self deny: (ws includes: o3 copy).

	ws add: o3 copy.
	self assert: ws size equals: 3
]

{ #category : #tests }
WeakIdentitySetTest >> testAddIdenticalElements [
	| identitySet aString anOtherString |
	aString := 'hello'.
	anOtherString := aString copy.

	self assert: aString equals: anOtherString.
	self deny: aString identicalTo: anOtherString.

	identitySet := self classToBeTested new.
	identitySet add: aString.

	self assert: (identitySet occurrencesOf: aString) equals: 1.
	self assert: (identitySet occurrencesOf: anOtherString) equals: 0.

	self assert: (identitySet includes: aString).
	self deny: (identitySet includes: anOtherString) equals: 0
]

{ #category : #tests }
WeakIdentitySetTest >> testAddIncludesSizeReclaim [
	"It is complex test in superclass to check how elements are garbage collected.
	And how #includes: behavios after that.
	The logic is based on WeakSet and it is not compatible to WeakIdentitySet.
	Adopting this test for WeakIdentitySest is not simple task"
]

{ #category : #tests }
WeakIdentitySetTest >> testCollisions [
	"It is complex test in superclass to check how collisions are resolved.
	The logic is based on WeakSet and it is not compatible to WeakIdentitySet.
	Adopting this test for WeakIdentitySest is not simple task"
]
