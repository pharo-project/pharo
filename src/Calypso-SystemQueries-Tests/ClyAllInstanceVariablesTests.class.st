Class {
	#name : #ClyAllInstanceVariablesTests,
	#superclass : #ClyVariableQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllInstanceVariablesTests >> createQuery [
	^ClyAllInstanceVariables from: ClyClassScope of: ClyClass2FromP1 in: environment
]

{ #category : #tests }
ClyAllInstanceVariablesTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass2FromP1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ProtoObject in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllInstanceVariablesTests >> testFromSingleClass [

	| varNames |
	self queryFromScope: ClyClassScope of: ClyClass2FromP1.
	
	varNames := resultItems collect: #name as: Set.
	self assert: varNames equals: ClyClass2FromP1 instVarNames asSet
]
