"
I am responsible for removing a message send inside a method. In this implementation, only the selector of the message is necessary to execute this transformation. Pay attention that I do not give any warranty on the result.
It means that you can break hard your code applying blindly this transformation.


Usage:

```
| transformation |
transformation := (RBRemoveAllMessageSendsTransformation
				messageSend: #isMessage
				inMethod: #privateTransform
				inClass: #RBRemoveAllMessageSendsTransformation)
				generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- Check whether this method ever calls the given message.

Observations about the transformation:
- If the method contains more than one message send like the one specified in this transformation, all of these message sends will be removed.
- Removing the message send means removing the receiver, too. The only exception occurs when the receiver is another message send (e.g., in a cascade)
"
Class {
	#name : 'RBRemoveAllMessageSendsTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'message'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveAllMessageSendsTransformation class >> messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	^ self new
		messageSend: aMessageSend
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'api' }
RBRemoveAllMessageSendsTransformation class >> model: aRBModel messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		messageSend: aMessageSend
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'preconditions' }
RBRemoveAllMessageSendsTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition
			   withBlock: [ self definingClass canUnderstand: selector ]
			   errorString:
			   ('Method named <1s> does not exist' expandMacrosWith: selector)).
		  (RBCondition
			   withBlock: [
				   (self definingMethod allChildren select: [ :each |
					    each isMessage ]) anySatisfy: [ :messageNode |
					   messageNode selector = message asSymbol ] ]
			   errorString:
				   ('Method named <1s> does not implement this message send <2s>'
					    expandMacrosWith: selector
					    with: message)) }
]

{ #category : 'scripting api - conditions' }
RBRemoveAllMessageSendsTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRemoveAllMessageSendsTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : 'api' }
RBRemoveAllMessageSendsTransformation >> messageSend: aMessageSend inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	message := aMessageSend
]

{ #category : 'executing' }
RBRemoveAllMessageSendsTransformation >> privateTransform [

	| methodTree nodesToModify |
	methodTree := self definingMethod.

	"try to find a message node with the same selector"
	nodesToModify := ( methodTree allChildren removeDuplicates
		select: [ :each | each isMessage] )
		select: [ :node | node selector = message asSymbol ].

	nodesToModify ifNotEmpty: [
		nodesToModify do: [:each | each removeFromTree].
		self definingClass compileTree: methodTree ]
]

{ #category : 'storing' }
RBRemoveAllMessageSendsTransformation >> storeOn: aStream [

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
