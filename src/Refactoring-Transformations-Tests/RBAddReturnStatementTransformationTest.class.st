Class {
	#name : 'RBAddReturnStatementTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBAddReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : 'utilities' }
RBAddReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testAccessIsNotDefined [

	self shouldFail: (RBAddReturnStatementTransformation
			 return: '^ variable2'
			 inMethod: #methodBefore
			 inClass: #RBAddReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testAlreadyDefinesReturn [

	self shouldFail: (RBAddReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #methodAfter
			 inClass: #RBAddReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #methodBefore
			 inClass: #RBReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
			 return: '^ variable'
			 inMethod: #method
			 inClass: #RBAddReturnStatementTransformationTest)
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddReturnStatementTransformation
		                return: '^ variable'
		                inMethod: #methodBefore
		                inClass: #RBAddReturnStatementTransformationTest).
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBAddReturnStatementTransformationTest.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : 'tests' }
RBAddReturnStatementTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddReturnStatementTransformation new
		                   return: '^ variable'
		                   inMethod: #methodBefore
		                   inClass: self class name).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]
