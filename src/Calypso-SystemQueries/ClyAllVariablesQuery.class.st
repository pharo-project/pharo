"
I return all type of variables from given scope.

Scope should support #variablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllVariablesQuery,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllVariablesQuery >> buildResult: aQueryResult [

	| allVars |
	allVars := OrderedCollection new.
	
	scope variablesDo: [ :each | allVars add: each ].
	
	aQueryResult fillWith: allVars
]

{ #category : #execution }
ClyAllVariablesQuery >> checkEmptyResult [

	scope variablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllVariablesQuery >> description [

	^'all variables'
]

{ #category : #testing }
ClyAllVariablesQuery >> retrievesItem: aVariable [
	^true
]

{ #category : #accessing }
ClyAllVariablesQuery >> shortName [
	^'Vars'
]
