Class {
	#name : 'RBRemoveTemporaryVariableTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foo
			 inClass: #RBTemporaryVariableTransformationTest)
]

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foofoo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := RBAddMethodTransformation
		                  sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
		                  in: self changeMockClass name
		                  withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		                  model: transformation model
		                  variable: 'temp'
		                  inMethod: #foo
		                  inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #foo).
	self
		assert: (class parseTreeForSelector: #foo) temporaries size
		equals: 1
]

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp123'
			 inMethod: #foo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]
