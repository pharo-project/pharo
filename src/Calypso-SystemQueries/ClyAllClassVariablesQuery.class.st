"
I return all class variables from given scope.

Scope should support #classVariablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllClassVariablesQuery,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllClassVariablesQuery >> buildResult: aQueryResult [

	| vars |
	vars := OrderedCollection new.
	
	scope classVariablesDo: [ :each | vars add: each ].
	
	aQueryResult fillWith: vars
]

{ #category : #execution }
ClyAllClassVariablesQuery >> checkEmptyResult [

	scope classVariablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllClassVariablesQuery >> description [

	^'class variables'
]

{ #category : #testing }
ClyAllClassVariablesQuery >> retrievesItem: aVariable [
	^aVariable isKindOf: ClyClassVariable 
]
