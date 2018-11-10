"
I return all instance variables from given scope.

Scope should support #instanceVariablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllInstanceVariables,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllInstanceVariables >> buildResult: aQueryResult [

	| vars |
	vars := OrderedCollection new.
	
	scope instanceVariablesDo: [ :each | vars add: each ].
	
	aQueryResult fillWith: vars
]

{ #category : #execution }
ClyAllInstanceVariables >> checkEmptyResult [

	scope instanceVariablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllInstanceVariables >> description [

	^'instance variables'
]

{ #category : #testing }
ClyAllInstanceVariables >> retrievesItem: aVariable [
	^aVariable isKindOf: ClyInstanceVariable 
]
