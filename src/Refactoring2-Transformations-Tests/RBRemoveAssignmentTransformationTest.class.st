Class {
	#name : #RBRemoveAssignmentTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #utilities }
RBRemoveAssignmentTransformationTest >> methodAfter [

	| variable |
	variable := 'String'
]

{ #category : #utilities }
RBRemoveAssignmentTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	variable := 1 asString
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testAssignmentDoesNotExist [

	| refactoring |
	refactoring := (RBAddTemporaryVariableTransformation
		                variable: 'variable2'
		                inMethod: #methodBefore
		                inClass: #RBRemoveAssignmentTransformationTest)
		               asRefactoring primitiveExecute.

	self shouldFail: (RBRemoveAssignmentTransformation
			 model: refactoring model
			 variable: 'variable2'
			 value: '1 asString'
			 inMethod: #methodBefore
			 inClass: #RBRemoveAssignmentTransformationTest) asRefactoring
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation
							variable: 'variable'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBAssignmentTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation
							variable: 'variable'
							value: '1 asString'
							inMethod: #method
							inClass: #RBRemoveAssignmentTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveAssignmentTransformation
		                variable: 'variable'
		                value: '1 asString'
		                inMethod: #methodBefore
		                inClass: #RBRemoveAssignmentTransformationTest)
		               asRefactoring primitiveExecute.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBRemoveAssignmentTransformationTest.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveAssignmentTransformation new
		                   variable: 'variable'
		                   value: '1 asString'
		                   inMethod: #methodBefore
		                   inClass: self class name) primitiveExecute.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : #tests }
RBRemoveAssignmentTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation
							variable: 'variable1'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBRemoveAssignmentTransformationTest)
							asRefactoring
]
