"
I return all class variables from given scope.

Scope should support #classVariablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllClassVariables,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllClassVariables >> buildResult: aQueryResult [

	| vars |
	vars := OrderedCollection new.
	
	scope classVariablesDo: [ :each | vars add: each ].
	
	aQueryResult fillWith: vars
]

{ #category : #execution }
ClyAllClassVariables >> checkEmptyResult [

	scope classVariablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllClassVariables >> description [

	^'class variables'
]

{ #category : #testing }
ClyAllClassVariables >> retrievesItem: aVariable [
	^aVariable isKindOf: ClyClassVariable 
]
