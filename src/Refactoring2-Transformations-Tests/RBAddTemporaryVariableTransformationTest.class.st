Class {
	#name : #RBAddTemporaryVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBAddTemporaryVariableTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddTemporaryVariableTransformation 
							variable: 'variable2'
							inMethod: #methodBefore
							inClass: #RBReturnStatementTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddTemporaryVariableTransformation 
							variable: 'variable2'
							inMethod: #method
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBAddTemporaryVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddTemporaryVariableTransformation
						variable: 'variable2'
						inMethod: #methodBefore
						inClass: #RBAddReturnStatementTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBAddReturnStatementTransformationTest.
	self assert: (class directlyDefinesMethod: #methodBefore).
	self assert: (class parseTreeFor: #methodBefore) temporaries size equals: 2
]

{ #category : #tests }
RBAddTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddTemporaryVariableTransformation
							variable: 'variable'
							inMethod: #one
							inClass: self changeMock name)
							transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #one).
	self assert: (class parseTreeFor: #one) temporaries size equals: 1
]

{ #category : #tests }
RBAddTemporaryVariableTransformationTest >> testVariableAlreadyExists [

	self shouldFail: (RBAddTemporaryVariableTransformation
							variable: 'variable'
							inMethod: #methodBefore
							inClass: #RBAddReturnStatementTransformationTest)
							asRefactoring
]
