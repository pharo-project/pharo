Class {
	#name : #DTCoverageMockTest,
	#superclass : #TestCase,
	#instVars : [
		'mockObj'
	],
	#category : #'DrTests-TestCoverage-Tests-Mocks-Base'
}

{ #category : #running }
DTCoverageMockTest >> setUp [

	super setUp.
	mockObj := MockForCoverage new
]

{ #category : #running }
DTCoverageMockTest >> testMethod1forMock [

	self assert: mockObj method1ForMock equals: 2
]

{ #category : #running }
DTCoverageMockTest >> testMethod2forMock [

	self
		assert: 5
		equals: 5
]
