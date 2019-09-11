"
I am responsible for adding a message send inside a method. In this implementation, all the message send, e.g., 'variable message: (arg)' shall be described as a string. It was implemented this way to avoid having another transformation just to add the receiver as a literal node.

Usage:
| transformation |
transformation := (RBAddMessageSendTransformation
				messageSend: 'variable byteAt: 1'
				inMethod: #methodBefore
				inClass: #RBAddMessageSendTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the method exists.
- all accesses in the message send, e.g., receiver and arguments, are previously defined in the method

Observations about the transformation:
- The transformation does not know the location on which it will place the message send. Therefore, it extracts all the accesses in the message to be added, and adds the message after all the accesses are assigned.
- If there is no access, the message send will be the first statement of the method.
"
Class {
	#name : #RBAddMessageSendTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'message'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddMessageSendTransformation class >> messageSend: aString inMethod: aSelector inClass: aClassName [

	^ self new
		messageSend: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddMessageSendTransformation class >> model: aRBModel messageSend: aString inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		messageSend: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddMessageSendTransformation >> messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	message := aMessageSend
	
]

{ #category : #preconditions }
RBAddMessageSendTransformation >> preconditions [
	^ (RBCondition
		withBlock: [ self definingClass isNotNil ]
		errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
		&
			(RBCondition
				withBlock: [ self definingClass canUnderstand: selector ]
				errorString: ('Method named <1s> does not exist' expandMacrosWith: selector))
		&
			(RBCondition
				withBlock: [ | messageNode |
					messageNode := self parserClass parseExpression: message.
					messageNode allVariables
						allSatisfy: [ :e | 
							(self definingMethod hasArgumentNamed: e name)
								or: [ self definingMethod hasTemporaryNamed: e name ] ] ]
				errorString:
					('Some variables in this message send are not defined in method named <1s>.'
						expandMacrosWith: selector))
]

{ #category : #executing }
RBAddMessageSendTransformation >> privateTransform [
	| methodTree messageNode assignments variables previousNode |
	methodTree := self definingMethod.
	messageNode := self parserClass parseExpression: message.

	"if the message requires variables, look for the right variables in this context"
	variables := messageNode allVariables.
	assignments := (methodTree allChildren select: #isAssignment)
		select: [ :each | variables includes: each variable ].
	previousNode := assignments detectMax: #stop.
	previousNode
		ifNil: [ methodTree body addNodeFirst: messageNode ]
		ifNotNil: [ previousNode parent addNode: messageNode after: previousNode ].
	class compileTree: methodTree
]

{ #category : #printing }
RBAddMessageSendTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' messageSend: ''';
		nextPutAll: message;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
