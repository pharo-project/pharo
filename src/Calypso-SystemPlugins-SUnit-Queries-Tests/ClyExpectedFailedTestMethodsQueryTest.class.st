Class {
	#name : #ClyExpectedFailedTestMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailedTestMethodsQueryTest >> createQuery [
	^ClyExpectedFailedTestMethodsQuery from: ClyClassScope of: ClyTestedClassTest in: environment
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyTestedClassTest >> #testExpectedFailure in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope 
		of: ClyTestedClassTest >> #testOne in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyTestedClassTest >> #testExpectedFailure. 
		ClyTestedClassTest >> #testOne}.
	
	self assert: foundSelectors sorted asArray equals: #(testExpectedFailure) 
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testNotSelectsMethodWithoutExpectedFailurePragma [
	
	self deny: (query selectsMethod: ClyTestedClassTest >> #testOne)
]

{ #category : #tests }
ClyExpectedFailedTestMethodsQueryTest >> testSelectsTestMethodWithExpectedFailurePragma [
	
	self assert: (query selectsMethod: ClyTestedClassTest >> #testExpectedFailure)
]
