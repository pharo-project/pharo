Class {
	#name : #RBAddMessageSendTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #utilities }
RBAddMessageSendTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable byteAt: 1.
]

{ #category : #utilities }
RBAddMessageSendTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
]

{ #category : #tests }
RBAddMessageSendTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
							messageSend: 'variable byteAt: 1'
							inMethod: #methodBefore
							inClass: #RBMessageSendTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddMessageSendTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
							messageSend: 'variable byteAt: 1'
							inMethod: #method
							inClass: #RBAddMessageSendTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddMessageSendTransformationTest >> testReceiverDoesNotExist [

	self shouldFail: (RBAddMessageSendTransformation
							messageSend: 'variable2 byteAt: 1'
							inMethod: #methodBefore
							inClass: #RBAddMessageSendTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddMessageSendTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddMessageSendTransformation
						messageSend: 'variable byteAt: 1'
						inMethod: #methodBefore
						inClass: #RBAddMessageSendTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBAddMessageSendTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #tests }
RBAddMessageSendTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMessageSendTransformation new
		messageSend: 'variable byteAt: 1'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]
