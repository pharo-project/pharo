"
Checks for uses of the classes True and False instead of the objects true and false.

true is the object true, while True is the class whose true is the sole instance.
"
Class {
	#name : #RBUsesTrueRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		'trueBinding',
		'falseBinding'
	],
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBUsesTrueRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUsesTrueRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UsesTrueRule'
]

{ #category : #running }
RBUsesTrueRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemUsages |
	aMethod methodClass isTrait ifTrue: [ ^ self ].
	problemUsages := aMethod ast allChildren select: [ :aNode | 
		                 aNode isVariable and: [ 
			                 aNode isGlobal and: [ 
				                 #( True False ) includes: aNode name ] ] ].
	problemUsages do: [ :ref | 
		aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: ref hint: ref name asString)]
]

{ #category : #accessing }
RBUsesTrueRule >> group [
	^ 'Bugs'
]

{ #category : #initialization }
RBUsesTrueRule >> initialize [
	super initialize.
	trueBinding := Smalltalk globals associationAt: #True.
	falseBinding := Smalltalk globals associationAt: #False
]

{ #category : #accessing }
RBUsesTrueRule >> name [
	^ 'Uses True/False instead of true/false'
]

{ #category : #accessing }
RBUsesTrueRule >> severity [
	^ #error
]
