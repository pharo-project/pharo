"
I am responsible for removing a message send inside a method. In this implementation, only the selector of the message is necessary to execute this transformation.

Usage:
| transformation |
transformation := (RBRemoveMessageSendTransformation
				messageSend: #byteAt:
				inMethod: #methodBefore
				inClass: #RBRemoveMessageSendTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- Check whether this method ever calls the given message.

Observations about the transformation:
- If the method contains more than one message send like the one specified in this transformation, all of these message sends will be removed.
- Removing the message send means removing the receiver, too. The only exception occurs when the receiver is another message send (e.g., in a cascade)
"
Class {
	#name : #RBRemoveMessageSendTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'message'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemoveMessageSendTransformation class >> messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	^ self new
		messageSend: aMessageSend
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMessageSendTransformation class >> model: aRBModel messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		messageSend: aMessageSend
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMessageSendTransformation >> messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	message := aMessageSend
]

{ #category : #preconditions }
RBRemoveMessageSendTransformation >> preconditions [

	^ (RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
	  & (RBCondition
			withBlock: [ self definingClass canUnderstand: selector ]
			errorString: ('Method named <1s> does not exist' expandMacrosWith: selector))
	  & (RBCondition
			withBlock: [ 
				(self definingMethod
					allChildren select: #isMessage)
					anySatisfy: [ :messageNode | messageNode selector = message asSymbol ] ]
			errorString: ('Method named <1s> does not implement this message send <2s>'
				expandMacrosWith: selector with: message))
]

{ #category : #executing }
RBRemoveMessageSendTransformation >> privateTransform [

	| methodTree nodesToModify |
	methodTree := self definingMethod.
	
	"try to find a message node with the same selector"
	nodesToModify := ( methodTree allChildren removeDuplicates
		select: #isMessage )
		select: [ :node | node selector = message asSymbol ].
	
	nodesToModify ifNotEmpty: [ 
		nodesToModify do: #removeFromTree.
		self definingClass compileTree: methodTree ]
]

{ #category : #printing }
RBRemoveMessageSendTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' messageSend: ''';
		nextPutAll: message asString;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
