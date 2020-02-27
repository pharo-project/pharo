"
I am responsible for adding a return statement inside a method. In this implementation, all the return value (including the '^') shall be described.

Usage: 
| transformation |
transformation := (RBAddReturnStatementTransformation
				return: '^ variable'
				inMethod: #methodBefore
				inClass: #RBAddReturnStatementTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the method does not have a return statement

Observations about the transformation:
- The transformation is straightforward: parse the return value and add the resulting node to the end of the method
"
Class {
	#name : #RBAddReturnStatementTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'returnValue'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddReturnStatementTransformation class >> model: aRBModel return: aString inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		return: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddReturnStatementTransformation class >> return: aString inMethod: aSelector inClass: aClassName [

	^ self new
		return: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBAddReturnStatementTransformation >> preconditions [
	^ (RBCondition
		withBlock: [ self definingClass isNotNil ]
		errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
		&
			(RBCondition
				withBlock: [ self definingClass canUnderstand: selector ]
				errorString: ('Method named <1s> does not exist' expandMacrosWith: selector))
		&
			(RBCondition
				withBlock: [ self definingMethod lastIsReturn not ]
				errorString:
					('Method named <1s> already has a return statement'
						expandMacrosWith: selector))
		&
			(RBCondition
				withBlock: [ | returnNode |
					returnNode := self parserClass parseExpression: returnValue.
					returnNode allVariables
						allSatisfy: [ :e | 
							(self definingMethod hasArgumentNamed: e name)
								or: [ self definingMethod hasTemporaryNamed: e name ] ] ]
				errorString:
					('Some variables in this return statement are not defined in method named <1s>.'
						expandMacrosWith: selector))
]

{ #category : #executing }
RBAddReturnStatementTransformation >> privateTransform [
	"the transformation is straighforward: just add the node at the last position of the method.
	 #todo the case in which the context is a block"

	| methodTree messageNode |
	methodTree := self definingMethod.
	messageNode := self parserClass parseExpression: returnValue.
	methodTree body addNode: messageNode.
	self definingClass compileTree: methodTree
]

{ #category : #api }
RBAddReturnStatementTransformation >> return: anExpression inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	returnValue := anExpression
]

{ #category : #printing }
RBAddReturnStatementTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' return: ''';
		nextPutAll: returnValue;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
