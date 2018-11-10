Class {
	#name : #ClyVariableReadersTests,
	#superclass : #ClyVariableReferencesTestCase,
	#instVars : [
		'var2'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyVariableReadersTests >> queryClass [
	^ClyVariableReaders
]

{ #category : #'methods for tests' }
ClyVariableReadersTests >> reader1OfVar1 [

	^readVar1
]

{ #category : #'methods for tests' }
ClyVariableReadersTests >> reader2OfVar1 [

	^readVar1
]

{ #category : #running }
ClyVariableReadersTests >> targetVarName [
	^#readVar1
]

{ #category : #tests }
ClyVariableReadersTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope of: self class >> #reader1OfVar1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: self class >> #writerOfVar1 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyVariableReadersTests >> testFromClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 2.	
	self assert: foundSelectors equals: #(reader1OfVar1 reader2OfVar1)
]

{ #category : #tests }
ClyVariableReadersTests >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #reader1OfVar1. self class >> #writerOfVar1 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reader1OfVar1)
]

{ #category : #tests }
ClyVariableReadersTests >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(reader1OfVar1 reader2OfVar1 supeclassReaderOfVar1)
]

{ #category : #'methods for tests' }
ClyVariableReadersTests >> writerOfVar1 [

	readVar1 := #testValue
]
