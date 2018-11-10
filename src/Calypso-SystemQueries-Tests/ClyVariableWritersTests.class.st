Class {
	#name : #ClyVariableWritersTests,
	#superclass : #ClyVariableReferencesTestCase,
	#instVars : [
		'var2'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyVariableWritersTests >> queryClass [
	^ClyVariableWriters
]

{ #category : #'methods for tests' }
ClyVariableWritersTests >> readerOfVar1 [

	^writtenVar1
]

{ #category : #running }
ClyVariableWritersTests >> targetVarName [
	^#writtenVar1
]

{ #category : #tests }
ClyVariableWritersTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope of: self class >> #writer1OfVar1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: self class >> #readerOfVar1 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyVariableWritersTests >> testFromClassScope [

	self queryFromScope: ClyBothMetaLevelClassScope of: self class.
	
	self assert: resultItems size equals: 2.
	self assert: foundSelectors equals: #(writer1OfVar1 writer2OfVar1)
]

{ #category : #tests }
ClyVariableWritersTests >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #writer1OfVar1. self class >> #readerOfVar1 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #writer1OfVar1)
]

{ #category : #tests }
ClyVariableWritersTests >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(supeclassWriterOfVar1 writer1OfVar1 writer2OfVar1)
]

{ #category : #'methods for tests' }
ClyVariableWritersTests >> writer1OfVar1 [

	writtenVar1 := #value1
]

{ #category : #'methods for tests' }
ClyVariableWritersTests >> writer2OfVar1 [

	writtenVar1 := #value2
]
