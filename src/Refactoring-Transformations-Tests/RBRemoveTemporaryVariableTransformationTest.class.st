Class {
	#name : 'RBRemoveTemporaryVariableTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foo
			 inClass: #RBTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foofoo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddMethodTransformation
		                sourceCode: 'foo
										| temp bar |
										bar := 5.
										temp := bar * bar.
										Transcript show: temp printString; cr.
										^temp * temp'
		                in: #RBRemoveTemporaryVariableTransformationTest
		                withProtocol: #accessing ) generateChanges.

	refactoring := (RBRemoveTemporaryVariableTransformation
		                model: refactoring model
		                variable: 'temp'
		                inMethod: #foo
		                inClass:
		                #RBRemoveTemporaryVariableTransformationTest)
		               generateChanges.

	self assert: refactoring model changes changes size equals: 2.

	class := refactoring model classNamed:
		         #RBRemoveTemporaryVariableTransformationTest.
	self assert: (class directlyDefinesMethod: #foo).
	self
		assert: (class parseTreeForSelector: #foo) temporaries size
		equals: 1
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
		                   sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
		                   in: self changeMockClass name
		                   withProtocol: #accessing ) generateChanges.

	transformation := (RBRemoveTemporaryVariableTransformation
		                   model: transformation model
		                   variable: 'temp'
		                   inMethod: #foo
		                   inClass: self changeMockClass name)
		                  generateChanges.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #one).
	self
		assert: (class parseTreeForSelector: #foo) temporaries size
		equals: 1
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp123'
			 inMethod: #foo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]
