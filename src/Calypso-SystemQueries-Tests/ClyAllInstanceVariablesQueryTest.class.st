Class {
	#name : #ClyAllInstanceVariablesQueryTest,
	#superclass : #ClyVariableQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllInstanceVariablesQueryTest >> createQuery [
	^ClyAllInstanceVariablesQuery from: ClyClassScope of: ClyClass2FromP1Mock in: environment
]

{ #category : #tests }
ClyAllInstanceVariablesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass2FromP1Mock in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ProtoObject in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllInstanceVariablesQueryTest >> testFromSingleClass [

	| varNames |
	self queryFromScope: ClyClassScope of: ClyClass2FromP1Mock.
	
	varNames := resultItems collect: #name as: Set.
	self assert: varNames equals: ClyClass2FromP1Mock instVarNames asSet
]
