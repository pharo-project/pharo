Class {
	#name : #ClyAllVariablesTests,
	#superclass : #ClyVariableQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllVariablesTests >> createQuery [
	^ClyAllVariables from: ClyClassScope of: ClyClass2FromP1 in: environment
]

{ #category : #tests }
ClyAllVariablesTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass2FromP1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ProtoObject in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllVariablesTests >> testFromSingleClass [

	| varNames |
	self queryFromScope: ClyClassScope of: ClyClass2FromP1.
	
	varNames := resultItems collect: #name as: Set.
	self 
		assert: varNames 
		equals: ClyClass2FromP1 instVarNames asSet, (ClyClass2FromP1 classVariables collect: #name)
]
