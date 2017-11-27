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
	"self run: #testGrowWithNil"
	"This test covers that grow take into account that nil are wrapped elements of sets"
	| set |
	set := IdentitySet new.
	set add: nil.
	set grow.
	self assert: (set includes: nil)
]

{ #category : #'tests - identity' }
IdentitySetTest >> testIdentity [
	"self run:#testIdentity" 
	"self debug:#testIdentity"
	| identitySet aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: (aString = anOtherString).
	self assert: (aString == anOtherString) not.

	
	identitySet := self classToBeTested  new.
	identitySet add: aString.

	
	self assert: (identitySet occurrencesOf: aString) = 1.
	self assert: (identitySet occurrencesOf: anOtherString) = 0.
	

	self assert: (identitySet includes: aString).
	self deny: (identitySet includes: anOtherString) = 0.
]

{ #category : #tests }
IdentitySetTest >> testIsHealthy [
	"we use associations as elements on purpose, because they change their
	hash when we change their keys.  But they don't change their _identityHash_, so
	we can't make an IdentitySet unhealthy by mutating them."

	| a1 a2 set |
	set := self classToBeTested new.
	a1 := 3 -> nil.
	a2 := nil -> 3.
	set
		add: a1;
		add: a2.
	self assert: set isHealthy.
	a1 key: 0.
	a2 key: 0.
	self assert: set isHealthy
	
]
