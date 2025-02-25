Class {
	#name : 'RBAddAssignmentTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBAddAssignmentTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable := 1 asString
]

{ #category : 'utilities' }
RBAddAssignmentTransformationTest >> methodBefore [

	| variable |
	variable := 'String'
]

{ #category : 'tests' }
RBAddAssignmentTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
			 variable: 'variable'
			 value: '1 asString'
			 inMethod: #methodBefore
			 inClass: #RBAssignmentTransformationTest)
]

{ #category : 'tests' }
RBAddAssignmentTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
			 variable: 'variable'
			 value: '1 asString'
			 inMethod: #method
			 inClass: #RBAddAssignmentTransformationTest)
]

{ #category : 'tests' }
RBAddAssignmentTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddAssignmentTransformation
		                variable: 'variable'
		                value: '1 asString'
		                inMethod: #methodBefore
		                inClass: #RBAddAssignmentTransformationTest).
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBAddAssignmentTransformationTest.
	self assert: (class directlyDefinesMethod: #methodBefore).
	self
		assert:
		(class parseTreeForSelector: #methodBefore) body statements size
		equals: 2.
	self
		assert:
			(class parseTreeForSelector: #methodBefore) body statements last
				value sourceCode
		equals: '1 asString'
]

{ #category : 'tests' }
RBAddAssignmentTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddAssignmentTransformation new
		                   variable: 'variable'
		                   value: '1 asString'
		                   inMethod: #methodBefore
		                   inClass: self class name).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : 'tests' }
RBAddAssignmentTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
			 variable: 'variable1'
			 value: '1 asString'
			 inMethod: #methodBefore
			 inClass: #RBAddAssignmentTransformationTest)
]
