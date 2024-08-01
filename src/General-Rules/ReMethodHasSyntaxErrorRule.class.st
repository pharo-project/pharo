"
Methods can be compiled with syntax errors, fix the code and recompile the method
"
Class {
	#name : 'ReMethodHasSyntaxErrorRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReMethodHasSyntaxErrorRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReMethodHasSyntaxErrorRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReMethodHasSyntaxErrorRule class >> ruleName [
	^ 'Method source has syntax errors'
]

{ #category : 'accessing' }
ReMethodHasSyntaxErrorRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReMethodHasSyntaxErrorRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'MethodSyntaxError'
]

{ #category : 'running' }
ReMethodHasSyntaxErrorRule >> basicCheck: aMethod [
	^aMethod isFaulty
]
