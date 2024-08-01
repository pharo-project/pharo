Class {
	#name : 'RBAddMessageSendTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBAddMessageSendTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable byteAt: 1
]

{ #category : 'utilities' }
RBAddMessageSendTransformationTest >> methodBefore [

	| variable |
	variable := 'String'
]

{ #category : 'tests' }
RBAddMessageSendTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
			 messageSend: 'variable byteAt: 1'
			 inMethod: #methodBefore
			 inClass: #RBMessageSendTransformationTest)
]

{ #category : 'tests' }
RBAddMessageSendTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
			 messageSend: 'variable byteAt: 1'
			 inMethod: #method
			 inClass: #RBAddMessageSendTransformationTest)
]

{ #category : 'tests' }
RBAddMessageSendTransformationTest >> testReceiverDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
			 messageSend: 'variable2 byteAt: 1'
			 inMethod: #methodBefore
			 inClass: #RBAddMessageSendTransformationTest)
]

{ #category : 'tests' }
RBAddMessageSendTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddMessageSendTransformation
		                messageSend: 'variable byteAt: 1'
		                inMethod: #methodBefore
		                inClass: #RBAddMessageSendTransformationTest).
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBAddMessageSendTransformationTest.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : 'tests' }
RBAddMessageSendTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMessageSendTransformation new
		                   messageSend: 'variable byteAt: 1'
		                   inMethod: #methodBefore
		                   inClass: self class name).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]
