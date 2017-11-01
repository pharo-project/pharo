"
Checks for uses of the classes True and False instead of the objects true and false.

true is the object true, while True is the class whose true is the sole instance.
"
Class {
	#name : #RBUsesTrueRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'trueBinding',
		'falseBinding'
	],
	#category : 'Refactoring-Critics-BlockRules'
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
RBUsesTrueRule >> basicCheck: aMethod [ 
	^ (aMethod refersToLiteral: trueBinding) or: [ aMethod refersToLiteral: falseBinding ]
]

{ #category : #running }
RBUsesTrueRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: 
		[ result
			addMethod: aMethod;
			searchStrings: #('True' 'False' ) ]
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
RBUsesTrueRule >> rationale [
	^ 'Checks for uses of the classes True and False instead of the objects true and false.'
]

{ #category : #accessing }
RBUsesTrueRule >> severity [
	^ #error
]
