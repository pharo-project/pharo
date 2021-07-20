Class {
	#name : #RBAddAssignmentTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #utilities }
RBAddAssignmentTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable := 1 asString.
]

{ #category : #utilities }
RBAddAssignmentTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
]

{ #category : #tests }
RBAddAssignmentTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
							variable: 'variable'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBAssignmentTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddAssignmentTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
							variable: 'variable'
							value: '1 asString'
							inMethod: #method
							inClass: #RBAddAssignmentTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddAssignmentTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddAssignmentTransformation
						variable: 'variable'
						value: '1 asString'
						inMethod: #methodBefore
						inClass: #RBAddAssignmentTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBAddAssignmentTransformationTest.
	self assert: (class directlyDefinesMethod: #methodBefore).
	self assert: (class parseTreeFor: #methodBefore) body statements size equals: 2.
	self assert: (class parseTreeFor: #methodBefore) body statements last value sourceCode equals: '1 asString'
]

{ #category : #tests }
RBAddAssignmentTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddAssignmentTransformation new
		variable: 'variable'
		value: '1 asString'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #tests }
RBAddAssignmentTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBAddAssignmentTransformation
							variable: 'variable1'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBAddAssignmentTransformationTest)
							asRefactoring
]
