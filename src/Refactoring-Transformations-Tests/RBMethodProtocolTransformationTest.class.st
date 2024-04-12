Class {
	#name : 'RBMethodProtocolTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBMethodProtocolTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBMethodProtocolTransformation
			 protocol: 'empty protocol 2'
			 inMethod: #someMethod
			 inClass: #RBDummyEmptyClass123)
]

{ #category : 'tests' }
RBMethodProtocolTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBMethodProtocolTransformation
			 protocol: 'empty protocol 2'
			 inMethod: #method
			 inClass: #RBDummyEmptyClass)
]

{ #category : 'tests' }
RBMethodProtocolTransformationTest >> testRefactoring [

	| refactoring |
	self deny: (RBDummyEmptyClass hasProtocol: 'empty protocol 2').

	refactoring := (RBMethodProtocolTransformation
		                protocol: 'empty protocol 2'
		                inMethod: #someMethod
		                inClass: #RBDummyEmptyClass) generateChanges.
	refactoring performChanges.

	self deny:
		(RBDummyEmptyClass protocolNamed: 'empty protocol 2') isEmpty.

	refactoring := (RBMethodProtocolTransformation
		                protocol: 'empty protocol 1'
		                inMethod: #someMethod
		                inClass: #RBDummyEmptyClass) generateChanges.
	refactoring performChanges.

	self deny: (RBDummyEmptyClass hasProtocol: 'empty protocol 2')
]

{ #category : 'tests' }
RBMethodProtocolTransformationTest >> testTransform [

	| transformation |
	self deny: (RBDummyEmptyClass hasProtocol: 'empty protocol 2').

	transformation := (RBMethodProtocolTransformation new
		                   protocol: 'empty protocol 2'
		                   inMethod: #someMethod
		                   inClass: #RBDummyEmptyClass) generateChanges.
	transformation performChanges.

	self deny:
		(RBDummyEmptyClass protocolNamed: 'empty protocol 2') isEmpty.

	transformation := (RBMethodProtocolTransformation new
		                   protocol: 'empty protocol 1'
		                   inMethod: #someMethod
		                   inClass: #RBDummyEmptyClass) generateChanges.
	transformation performChanges.
	self deny: (RBDummyEmptyClass hasProtocol: 'empty protocol 2')
]
