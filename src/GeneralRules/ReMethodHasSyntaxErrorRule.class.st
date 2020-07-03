"
Methods can be compiled with syntax errors, fix the code and recompile the method
"
Class {
	#name : #ReMethodHasSyntaxErrorRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReMethodHasSyntaxErrorRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReMethodHasSyntaxErrorRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MethodSyntaxError'
]

{ #category : #running }
ReMethodHasSyntaxErrorRule >> basicCheck: aMethod [
	^aMethod ast isFaulty
]

{ #category : #accessing }
ReMethodHasSyntaxErrorRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReMethodHasSyntaxErrorRule >> name [
	^ 'Method source has syntax errors'
]

{ #category : #accessing }
ReMethodHasSyntaxErrorRule >> severity [
	^ #error
]
