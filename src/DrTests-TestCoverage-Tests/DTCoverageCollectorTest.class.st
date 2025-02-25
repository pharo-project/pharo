Class {
	#name : 'DTCoverageCollectorTest',
	#superclass : 'TestCase',
	#category : 'DrTests-TestCoverage-Tests',
	#package : 'DrTests-TestCoverage-Tests'
}

{ #category : 'tests' }
DTCoverageCollectorTest >> testResultIsADTCoverageResultClass [

	| cov res |
	cov := DTCoverageCollector new.
	cov methods: { (MockForCoverage >> #method1ForMock) . (MockForCoverage >> #method2ForMock:) }.
	res := cov runOn: [ |mockClass| mockClass := MockForCoverage new.  mockClass method1ForMock ].

	self assert: res class equals: DTCoverageResult .
]

{ #category : 'tests' }
DTCoverageCollectorTest >> testReturnCoverageResult [

	| cov res |
	cov := DTCoverageCollector new.
	cov methods: {
			(MockForCoverage >> #method1ForMock).
			(MockForCoverage >> #method2ForMock:).
			(MockForCoverage >> #method3ForMockWithConditional:)}.
	res := cov runOn: [ 
		       | mockClass |
		       mockClass := MockForCoverage new.
		       mockClass method1ForMock.
		       mockClass method3ForMockWithConditional: 0 ].

	self assert: res methods size equals: 2.
	self assert: res partiallyCoveredMethods size equals: 1
]
