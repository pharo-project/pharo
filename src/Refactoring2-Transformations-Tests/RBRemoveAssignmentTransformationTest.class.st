Class {
	#name : #RBRemoveAssignmentTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'testing-utilities' }
RBRemoveAssignmentTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
]

{ #category : #'testing-utilities' }
RBRemoveAssignmentTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	variable := 1 asString.
]

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testAssignmentDoesNotExist [

	| refactoring |
	refactoring := (RBAddTemporaryVariableTransformation 
						variable: 'variable2'
						inMethod: #methodBefore
						inClass: #RBRemoveAssignmentTransformationTest)
						asRefactoring transform.

	self shouldFail: (RBRemoveAssignmentTransformation 
							model: refactoring model
							variable: 'variable2'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBRemoveAssignmentTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation 
							variable: 'variable'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBAssignmentTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation 
							variable: 'variable'
							value: '1 asString'
							inMethod: #method
							inClass: #RBRemoveAssignmentTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveAssignmentTransformation
						variable: 'variable'
						value: '1 asString'
						inMethod: #methodBefore
						inClass: #RBRemoveAssignmentTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBRemoveAssignmentTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveAssignmentTransformation new
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

{ #category : #testing }
RBRemoveAssignmentTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveAssignmentTransformation
							variable: 'variable1'
							value: '1 asString'
							inMethod: #methodBefore
							inClass: #RBRemoveAssignmentTransformationTest)
							asRefactoring
]
