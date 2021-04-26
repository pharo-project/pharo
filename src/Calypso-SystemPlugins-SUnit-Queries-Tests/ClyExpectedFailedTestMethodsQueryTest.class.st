Class {
	#name : #ClyExpectedFailedTestMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailedTestMethodsQueryTest >> createQuery [
	^ClyExpectedFailedTestMethodsQuery from: ClyClassScope of: ClyTestedClassMockTest in: environment
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyTestedClassMockTest >> #testExpectedFailure in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope 
		of: ClyTestedClassMockTest >> #testOne in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyTestedClassMockTest >> #testExpectedFailure. 
		ClyTestedClassMockTest >> #testOne}.
	
	self assert: foundSelectors sorted asArray equals: #(testExpectedFailure) 
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testNotSelectsMethodWithoutExpectedFailurePragma [
	
	self deny: (query selectsMethod: ClyTestedClassMockTest >> #testOne)
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testSelectsTestMethodWithExpectedFailurePragma [
	
	self assert: (query selectsMethod: ClyTestedClassMockTest >> #testExpectedFailure)
]
