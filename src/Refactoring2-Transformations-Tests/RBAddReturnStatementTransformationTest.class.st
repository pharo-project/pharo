Class {
	#name : #RBAddReturnStatementTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #utilities }
RBAddReturnStatementTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	^ variable
]

{ #category : #utilities }
RBAddReturnStatementTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
]

{ #category : #tests }
RBAddReturnStatementTransformationTest >> testAccessIsNotDefined [

	self shouldFail: (RBAddReturnStatementTransformation 
							return: '^ variable2'
							inMethod: #methodBefore
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddReturnStatementTransformationTest >> testAlreadyDefinesReturn [

	self shouldFail: (RBAddReturnStatementTransformation 
							return: '^ variable'
							inMethod: #methodAfter
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBAddReturnStatementTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
							return: '^ variable'
							inMethod: #methodBefore
							inClass: #RBReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBAddReturnStatementTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddReturnStatementTransformation
							return: '^ variable'
							inMethod: #method
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #tests }
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

{ #category : #tests }
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
