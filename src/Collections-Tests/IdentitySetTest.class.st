"
SUnit tests for identity sets
"
Class {
	#name : #IdentitySetTest,
	#superclass : #SetTest,
	#instVars : [
		'floatCollection'
	],
	#category : #'Collections-Tests-Unordered'
}

{ #category : #'building suites' }
IdentitySetTest class >> shouldInheritSelectors [

^true
]

{ #category : #requirements }
IdentitySetTest >> classToBeTested [

	^ IdentitySet
]

{ #category : #requirements }
IdentitySetTest >> identityCollectionWithElementsCopyNotIdentical [
" return a collection including elements for which #copy return a new object "
	^ floatCollection ifNil: [ floatCollection := IdentitySet new add: 2.5 ; add: 4.5 ; add:5.5 ; yourself ].
]

{ #category : #'tests - identity' }
IdentitySetTest >> testGrowWithNil [
	"This test covers that grow take into account that nil are wrapped elements of sets"
	| set |
	set := IdentitySet new.
	set add: nil.
	set grow.
	self assert: (set includes: nil)
]

{ #category : #'tests - identity' }
IdentitySetTest >> testIdentity [
	| identitySet aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: aString equals: anOtherString.
	self deny: aString == anOtherString.
	
	identitySet := self classToBeTested  new.
	identitySet add: aString.
	
	self assert: (identitySet occurrencesOf: aString) equals: 1.
	self assert: (identitySet occurrencesOf: anOtherString) equals: 0.
	
	self assert: (identitySet includes: aString).
	self deny: (identitySet includes: anOtherString) equals: 0.
]
