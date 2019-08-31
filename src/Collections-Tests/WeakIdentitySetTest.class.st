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
WeakIdentitySetTest >> testGrowWithNil [
	"This test covers that grow take into account that nil are wrapped elements of sets"
	| set |
	set := IdentitySet new.
	set add: nil.
	set grow.
	self assert: (set includes: nil)
]

{ #category : #tests }
WeakIdentitySetTest >> testIdentity [
	| identitySet aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: aString equals: anOtherString.
	self deny: aString == anOtherString.
	
	identitySet := self classToBeTested new.
	identitySet add: aString.
	
	self assert: (identitySet occurrencesOf: aString) equals: 1.
	self assert: (identitySet occurrencesOf: anOtherString) equals: 0.
	
	self assert: (identitySet includes: aString).
	self deny: (identitySet includes: anOtherString) equals: 0.
]
