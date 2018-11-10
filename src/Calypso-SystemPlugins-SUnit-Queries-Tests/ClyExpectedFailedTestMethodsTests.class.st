Class {
	#name : #ClyExpectedFailedTestMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailedTestMethodsTests >> createQuery [
	^ClyExpectedFailedTestMethods from: ClyClassScope of: ClyTestedClassTests in: environment
]

{ #category : #tests }
ClyExpectedFailedTestMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyTestedClassTests >> #testExpectedFailure in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope 
		of: ClyTestedClassTests >> #testOne in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExpectedFailedTestMethodsTests >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyTestedClassTests >> #testExpectedFailure. 
		ClyTestedClassTests >> #testOne}.
	
	self assert: foundSelectors sorted asArray equals: #(testExpectedFailure) 
]

{ #category : #tests }
ClyExpectedFailedTestMethodsTests >> testNotSelectsMethodWithoutExpectedFailurePragma [
	
	self deny: (query selectsMethod: ClyTestedClassTests >> #testOne)
]

{ #category : #tests }
ClyExpectedFailedTestMethodsTests >> testSelectsTestMethodWithExpectedFailurePragma [
	
	self assert: (query selectsMethod: ClyTestedClassTests >> #testExpectedFailure)
]
