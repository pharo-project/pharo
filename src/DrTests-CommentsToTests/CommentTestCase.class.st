"
I am a TestCase created from a comment.
To create a new instance you can call one of my factory methods, if you want to understand it better you can look at the example in my class side.

"
Class {
	#name : #CommentTestCase,
	#superclass : #TestCase,
	#instVars : [
		'currentValue',
		'expectedValue',
		'expression',
		'classExample',
		'selectorExample'
	],
	#category : #'DrTests-CommentsToTests'
}

{ #category : #'instance creation' }
CommentTestCase class >> comment: stringComment class: aClass selector: aSymbol [

	| result |
	self flag: #todo.
	"this method body should be moved on instance side."
	result := [ Smalltalk compiler evaluate: stringComment ]
		on: Exception
		do: [ ^ self errorComment: stringComment class: aClass selector: aSymbol ].
	(result isKindOf: Association)
		ifFalse: [ ^ self errorComment: stringComment class: aClass selector: aSymbol ]
		ifTrue: [ ^ self new
				expression: stringComment;
				expectedValue: result key;
				currentValue: result value;
				setTestSelector: #testIt;
				classExample: aClass;
				selectorExample: aSymbol;
				yourself ]
]

{ #category : #'instance creation' }
CommentTestCase class >> errorComment: stringComment class: aClass selector: aSymbol [
	self flag: #todo.
	"this method body should be moved on instance side."
	^ self new
		expression: stringComment;
		setTestSelector: #testError;
		classExample: aClass;
		expectedValue: nil;
		currentValue: nil;
		selectorExample: aSymbol;
		yourself
]

{ #category : #example }
CommentTestCase class >> example [
	"1 + 1 >>> 2"
	^ CommentTestCase comment: '1 + 1 >>> 2' class: CommentTestCase class selector: #example
]

{ #category : #accessing }
CommentTestCase class >> testSelectors [
	self flag: #todo.
	"missing comments. I have no ide what this method is doing."
	^ super testSelectors \ {#testError . #testIt}
]

{ #category : #accessing }
CommentTestCase >> classExample [ 
	^ classExample
]

{ #category : #accessing }
CommentTestCase >> classExample: anObject [ 
	classExample := anObject
]

{ #category : #accessing }
CommentTestCase >> cleanUpInstanceVariables [
	self class allInstVarNames
		do: [ :name | 
			(#('testSelector' 'currentValue' 'expression' 'classExample' 'selectorExample')
				includes: name)
				ifFalse: [ self instVarNamed: name put: nil ] ]
]

{ #category : #accessing }
CommentTestCase >> currentValue [
	^ currentValue
]

{ #category : #accessing }
CommentTestCase >> currentValue: anObject [
	currentValue := anObject
]

{ #category : #accessing }
CommentTestCase >> drTestsBrowse [
	self flag: #todo.
	"this should be done via the presenter application and not using a global variable."
	Smalltalk tools browser openOnClass: classExample  selector: selectorExample
]

{ #category : #accessing }
CommentTestCase >> drTestsName [
	^ self asString
]

{ #category : #accessing }
CommentTestCase >> expectedValue [
	^ expectedValue
]

{ #category : #accessing }
CommentTestCase >> expectedValue: anObject [
	expectedValue := anObject
]

{ #category : #accessing }
CommentTestCase >> expression [
	^ expression
]

{ #category : #accessing }
CommentTestCase >> expression: anObject [
	expression := anObject
]

{ #category : #accessing }
CommentTestCase >> printString [
	^ expression
]

{ #category : #accessing }
CommentTestCase >> selectorExample [
	^ selectorExample
]

{ #category : #accessing }
CommentTestCase >> selectorExample: anObject [
	selectorExample := anObject
]

{ #category : #accessing }
CommentTestCase >> testError [
	self error: 'syntax error on the comment'
]

{ #category : #accessing }
CommentTestCase >> testIt [
	self assert: expectedValue equals: currentValue
]
