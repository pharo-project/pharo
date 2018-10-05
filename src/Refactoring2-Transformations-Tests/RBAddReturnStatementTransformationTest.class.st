Class {
	#name : #RBAddReturnStatementTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'testing-utilities' }
RBAddReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : #'testing-utilities' }
RBAddReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testAccessIsNotDefined [

	self shouldFail: (RBAddReturnStatementTransformation 
							return: '^ variable2'
							inMethod: #methodBefore
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testAlreadyDefinesReturn [

	self shouldFail: (RBAddReturnStatementTransformation 
							return: '^ variable'
							inMethod: #methodAfter
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodBefore
							inClass: #RBReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
							return: '^ variable'
							inMethod: #method
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddReturnStatementTransformation
						return: '^ variable'
						inMethod: #methodBefore
						inClass: #RBAddReturnStatementTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBAddReturnStatementTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #testing }
RBAddReturnStatementTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddReturnStatementTransformation new
		return: '^ variable'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]
