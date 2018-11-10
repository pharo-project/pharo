"
I return all type of variables from given scope.

Scope should support #variablesDo:.
Now only class scopes support it.
"
Class {
	#name : #ClyAllVariables,
	#superclass : #ClyVariableQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyAllVariables >> buildResult: aQueryResult [

	| allVars |
	allVars := OrderedCollection new.
	
	scope variablesDo: [ :each | allVars add: each ].
	
	aQueryResult fillWith: allVars
]

{ #category : #execution }
ClyAllVariables >> checkEmptyResult [

	scope variablesDo: [ :each | ^false ].
	
	^true
]

{ #category : #printing }
ClyAllVariables >> description [

	^'all variables'
]

{ #category : #testing }
ClyAllVariables >> retrievesItem: aVariable [
	^true
]

{ #category : #accessing }
ClyAllVariables >> shortName [
	^'Vars'
]
