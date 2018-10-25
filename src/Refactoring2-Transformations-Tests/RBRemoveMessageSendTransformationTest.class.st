Class {
	#name : #RBRemoveMessageSendTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'testing-utilities' }
RBRemoveMessageSendTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
]

{ #category : #'testing-utilities' }
RBRemoveMessageSendTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
	variable byteAt: 1.
]

{ #category : #testing }
RBRemoveMessageSendTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveMessageSendTransformation 
							messageSend: #byteAt:
							inMethod: #methodBefore
							inClass: #RBMessageSendTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveMessageSendTransformationTest >> testMessageDoesNotExist [

	self shouldFail: (RBRemoveMessageSendTransformation 
							messageSend: #byteAt123:
							inMethod: #methodBefore
							inClass: #RBRemoveMessageSendTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveMessageSendTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveMessageSendTransformation 
							messageSend: #byteAt:
							inMethod: #method
							inClass: #RBRemoveMessageSendTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemoveMessageSendTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveMessageSendTransformation
						messageSend: #byteAt:
						inMethod: #methodBefore
						inClass: #RBRemoveMessageSendTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBRemoveMessageSendTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #testing }
RBRemoveMessageSendTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveMessageSendTransformation new
		messageSend: #byteAt:
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]
