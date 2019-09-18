Class {
	#name : #DTTestCoverageTestForCoverage,
	#superclass : #TestCase,
	#instVars : [
		'mockObj'
	],
	#category : #'DrTests-TestCoverage-MocksForTests'
}

{ #category : #running }
DTTestCoverageTestForCoverage >> setUp [
	super setUp.
	mockObj := MockForCoverage new.

	
]

{ #category : #running }
DTTestCoverageTestForCoverage >> testMethod1forMock [
	self assert: mockObj method1ForMock equals: 2
]

{ #category : #running }
DTTestCoverageTestForCoverage >> testMethod2forMock [
	self
		assert: 5
		equals: 5
]
