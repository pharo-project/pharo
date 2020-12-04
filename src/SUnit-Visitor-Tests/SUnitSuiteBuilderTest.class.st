Class {
	#name : #SUnitSuiteBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'visitor'
	],
	#category : #'SUnit-Visitor-Tests'
}

{ #category : #running }
SUnitSuiteBuilderTest >> setUp [
	super setUp.
	visitor := SUnitSuiteBuilder new
]

{ #category : #tests }
SUnitSuiteBuilderTest >> testVisitingCaseReturnsSameCase [
	| result |
	result := visitor visit: self.

	self assert: result equals: self
]

{ #category : #tests }
SUnitSuiteBuilderTest >> testVisitingClassReturnsSuiteWithSameTestsSelectors [

	| result |
	result := visitor visit: self class.
	self
		assertCollection: (result tests collect: [:each | each selector])
		hasSameElements: self class testSelectors
]

{ #category : #tests }
SUnitSuiteBuilderTest >> testVisitingPackageReturnsSuiteOfSuites [
	| result testClasses |
	result := visitor visit: self class package.
	testClasses := self class package classes select: #isTestCase.
	self
		assert: result name
		equals: self class package name.
	self
		assertCollection: (result tests collect: [:each | each name])
		hasSameElements: (testClasses collect: [:each | each name]).
	self
		assertCollection: (result tests flatCollect: [:each | each tests collect: [:t | t selector]])
		hasSameElements: (testClasses flatCollect: [:each | each testSelectors])
]

{ #category : #tests }
SUnitSuiteBuilderTest >> testVisitingSuiteReturnsSameSuite [
	| suite result |
	suite := self class suite.
	result := visitor visit: suite.

	self assert: result equals: suite
]
