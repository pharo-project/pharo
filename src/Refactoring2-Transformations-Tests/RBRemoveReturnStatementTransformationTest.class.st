Class {
	#name : #RBRemoveReturnStatementTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #utilities }
RBRemoveReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'
]

{ #category : #utilities }
RBRemoveReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : #tests }
RBRemoveReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodBefore
							inClass: #RBReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBRemoveReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
							return: '^ variable'
							inMethod: #method
							inClass: #RBRemoveReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBRemoveReturnStatementTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveReturnStatementTransformation
						return: '^ variable'
						inMethod: #methodBefore
						inClass: #RBRemoveReturnStatementTransformationTest)
						asRefactoring transform.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed: #RBRemoveReturnStatementTransformationTest.
	self assert: (class parseTreeForSelector: #methodBefore) body
			equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : #tests }
RBRemoveReturnStatementTransformationTest >> testReturnDoesNotExist [

	self shouldFail: (RBRemoveReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodAfter
							inClass: #RBRemoveReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBRemoveReturnStatementTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveReturnStatementTransformation new
		return: '^ variable'
		inMethod: #methodBefore
		inClass: self class name)
		transform.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self assert: (class parseTreeForSelector: #methodBefore) body
			equals: (class parseTreeForSelector: #methodAfter) body
]
