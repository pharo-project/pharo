Class {
	#name : 'RBRemoveReturnStatementTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBRemoveReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'
]

{ #category : 'utilities' }
RBRemoveReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : 'tests' }
RBRemoveReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #methodBefore
			 inClass: #RBReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBRemoveReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #method
			 inClass: #RBRemoveReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBRemoveReturnStatementTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := RBRemoveReturnStatementTransformation
		               return: '^ variable'
		               inMethod: #methodBefore
		               inClass: #RBRemoveReturnStatementTransformationTest.
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBRemoveReturnStatementTransformationTest.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : 'tests' }
RBRemoveReturnStatementTransformationTest >> testReturnDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #methodAfter
			 inClass: #RBRemoveReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBRemoveReturnStatementTransformationTest >> testTransform [

	| transformation class |
	transformation := RBRemoveReturnStatementTransformation new
		                  return: '^ variable'
		                  inMethod: #methodBefore
		                  inClass: self class name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]
