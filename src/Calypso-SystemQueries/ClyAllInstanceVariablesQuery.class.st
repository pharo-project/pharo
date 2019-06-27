"
I return all instance variables from given scope.

Scope should support #instanceVariablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllInstanceVariablesQuery,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllInstanceVariablesQuery >> buildResult: aQueryResult [

	| vars |
	vars := OrderedCollection new.
	
	scope instanceVariablesDo: [ :each | vars add: each ].
	
	aQueryResult fillWith: vars
]

{ #category : #execution }
ClyAllInstanceVariablesQuery >> checkEmptyResult [

	scope instanceVariablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllInstanceVariablesQuery >> description [

	^'instance variables'
]

{ #category : #testing }
ClyAllInstanceVariablesQuery >> retrievesItem: aVariable [
	^aVariable isKindOf: ClyInstanceVariable 
]
