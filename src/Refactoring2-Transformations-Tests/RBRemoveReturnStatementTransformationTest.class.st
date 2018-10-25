Class {
	#name : #RBRemoveReturnStatementTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'testing-utilities' }
RBRemoveReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
]

{ #category : #'testing-utilities' }
RBRemoveReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : #testing }
RBRemoveReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodBefore
							inClass: #RBReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #testing }
RBRemoveReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation 
							return: '^ variable'
							inMethod: #method
							inClass: #RBRemoveReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveReturnStatementTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveReturnStatementTransformation 
						return: '^ variable'
						inMethod: #methodBefore
						inClass: #RBRemoveReturnStatementTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBRemoveReturnStatementTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #testing }
RBRemoveReturnStatementTransformationTest >> testReturnDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodAfter
							inClass: #RBRemoveReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #testing }
RBRemoveReturnStatementTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveReturnStatementTransformation new
		return: '^ variable'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]
