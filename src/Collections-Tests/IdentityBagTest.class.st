"
SUnit tests for identity bags
"
Class {
	#name : #IdentityBagTest,
	#superclass : #BagTest,
	#instVars : [
		'equalNotIdenticalElement',
		'elementToCopy',
		'identityBagNonEmptyNoDuplicate5Elements'
	],
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
IdentityBagTest >> elementToCopy [
	^ elementToCopy ifNil: [ elementToCopy := 'element to copy' ]
]

{ #category : #requirements }
IdentityBagTest >> equalNotIdenticalElement [
	^ equalNotIdenticalElement ifNil: [ equalNotIdenticalElement := self elementToCopy copy ]
]

{ #category : #requirements }
IdentityBagTest >> identityCollectionWithElementsCopyNotIdentical [
"Returns a collection including elements for which #copy doesn't return the same object."
	^ identityBagNonEmptyNoDuplicate5Elements ifNil: [ 
	identityBagNonEmptyNoDuplicate5Elements := IdentityBag new add: 2.5 ; add: 1.5  ;add: 5.5 ; yourself ]
]

{ #category : #requirements }
IdentityBagTest >> speciesClass [
	^ IdentityBag
]

{ #category : #'tests - converting' }
IdentityBagTest >> testAsSetWithEqualsElements [
	| t1 |
	t1 := self withEqualElements asSet.
	self withEqualElements do: [ :t2 | self assert: (t1 occurrencesOf: t2) equals: 1 ].
	self assert: t1 class equals: IdentitySet
]

{ #category : #tests }
IdentityBagTest >> testIdentity [
 
	| bag identityBag aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: aString equals: anOtherString.
	self assert: (aString == anOtherString) not.

	bag := Bag new.
	bag add: aString.
	bag add: aString.
	bag add: anOtherString.
	self assert: (bag occurrencesOf: aString) equals: 3.
	self assert: (bag occurrencesOf: anOtherString) equals: 3.
	
	identityBag := IdentityBag new.
	identityBag add: aString.
	identityBag add: aString.
	identityBag add: anOtherString.
	
	self assert: (identityBag occurrencesOf: aString) equals: 2.
	self assert: (identityBag occurrencesOf: anOtherString) equals: 1.




]

{ #category : #'tests - identity adding' }
IdentityBagTest >> testIdentityAdd [
	| added |
	self collection add: self elementToCopy.
	self deny: (self collection includes: self equalNotIdenticalElement).

	added := self collection add: self equalNotIdenticalElement.
	self assert: added identicalTo: self equalNotIdenticalElement.
	self assert: (self collection includes: self equalNotIdenticalElement)
]
