Class {
	#name : #ClyAllClassVariablesQueryTest,
	#superclass : #ClyVariableQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllClassVariablesQueryTest >> createQuery [
	^ClyAllClassVariablesQuery from: ClyClassScope of: ClyClass2FromP1Mock in: environment
]

{ #category : #tests }
ClyAllClassVariablesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass2FromP1Mock in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ProtoObject in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllClassVariablesQueryTest >> testFromSingleClass [

	| varNames |
	self queryFromScope: ClyClassScope of: ClyClass2FromP1Mock.
	
	varNames := resultItems collect: #name as: Set.
	self assert: varNames equals: (ClyClass2FromP1Mock classVariables collect: #name as: Set)
]
