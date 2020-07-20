"
Checks for uses of the classes True and False instead of the objects true and false.

true is the object true, while True is the class whose true is the sole instance.
"
Class {
	#name : #ReUsesTrueRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUsesTrueRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUsesTrueRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UsesTrueRule'
]

{ #category : #running }
ReUsesTrueRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemUsages |
	problemUsages := aMethod ast variableNodes select: [ :aNode | 
			                 aNode isGlobalVariable and: [ #( True False ) includes: aNode name ] ].
	problemUsages do: [ :ref | 
		aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: ref hint: ref name asString)]
]

{ #category : #accessing }
ReUsesTrueRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReUsesTrueRule >> name [
	^ 'Uses True/False instead of true/false'
]

{ #category : #accessing }
ReUsesTrueRule >> severity [
	^ #error
]
