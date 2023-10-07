Class {
	#name : 'DTCoverageMockTest',
	#superclass : 'TestCase',
	#instVars : [
		'mockObj'
	],
	#category : 'DrTests-TestCoverage-Tests-Mocks-Base',
	#package : 'DrTests-TestCoverage-Tests-Mocks',
	#tag : 'Base'
}

{ #category : 'running' }
DTCoverageMockTest >> setUp [

	super setUp.
	mockObj := MockForCoverage new
]

{ #category : 'running' }
DTCoverageMockTest >> testMethod1forMock [

	self assert: mockObj method1ForMock equals: 2
]

{ #category : 'running' }
DTCoverageMockTest >> testMethod2forMock [

	self
		assert: 5
		equals: 5
]

{ #category : 'running' }
DTCoverageMockTest >> testMethod3forMock [

	self assert: (mockObj method3ForMockWithConditional: 2 ) equals: 0
]
