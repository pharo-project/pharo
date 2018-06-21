"
A RubSmalltalkEditorTest is a test class for testing the behavior of RubSmalltalkEditor
"
Class {
	#name : #RubSmalltalkEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'source',
		'selection'
	],
	#category : #'Rubric-Tests'
}

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeError [
	| result |
	result := RubSmalltalkEditor new
		bestNodeInString: self source
		at: self selection
		edittingMode: RubSmalltalkCodeMode new
		shouldFavourExpressions: true
		onError: [ ^ true ].

	self
		assert: false
		description: 'Should have got an error not: ' , result printString
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeSelector: aSelector [
	| node |
	
	node := self exectuteBestNodeFor: aSelector.

	self assert: node selector equals: aSelector
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeSelector: aSelector description: aString [
	self assertNodeSelector: aSelector
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeValue: anObject [
	| node |
	
	node := self exectuteBestNodeFor: anObject printString.

	self assert: node value equals: anObject 
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeValue: anObject description: aString [
	self assertNodeValue: anObject 
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertNodeVariable: anObject [
	| node |
	
	node := self exectuteBestNodeFor: anObject printString.

	self assert: node name equals: anObject 
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertPlaygroundNodeSelector: aSelector [
	| node |
	
	node := self exectuteBestPlaygroundNodeFor: aSelector.

	self assert: node selector equals: aSelector
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertPlaygroundNodeValue: anObject [
	| node |
	
	node := self exectuteBestPlaygroundNodeFor: anObject printString.

	self assert: node value printString equals: (anObject isString ifTrue: [anObject ] ifFalse: [anObject printString])
]

{ #category : #helpers }
RubSmalltalkEditorTest >> exectuteBestNodeFor: aSelector [
	^ self exectuteBestNodeFor: aSelector edittingMode: RubSmalltalkCodeMode new
]

{ #category : #helpers }
RubSmalltalkEditorTest >> exectuteBestNodeFor: aSelector edittingMode: aRubEdittingMode [
	^ RubSmalltalkEditor new
		bestNodeInString: self source
		at: self selection
		edittingMode: aRubEdittingMode
		shouldFavourExpressions: true
		onError: [ self assert: false description: 'node not found for: ' , aSelector ]
]

{ #category : #helpers }
RubSmalltalkEditorTest >> exectuteBestPlaygroundNodeFor: aSelector [
	^ self exectuteBestNodeFor: aSelector edittingMode: RubSmalltalkScriptingMode new
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionAfter: aString [
	| pos |
	
	pos := (self source findString: aString) + aString size.
	self selection: (pos to: pos).
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionBefore: aString [
	self positionBefore: aString offset: 0
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionBefore: aString offset: aNumber [
	| pos |
	
	pos := (self source findString: aString) + aNumber.
	self selection: (pos to: pos).
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionOn: aString [
	| pos |
	
	pos := (self source findString: aString).
	self selection: (pos to: pos + aString size).
	
]

{ #category : #accessing }
RubSmalltalkEditorTest >> selection [
	^ selection
]

{ #category : #accessing }
RubSmalltalkEditorTest >> selection: anObject [
	selection := anObject
]

{ #category : #accessing }
RubSmalltalkEditorTest >> source [
	^ source
]

{ #category : #accessing }
RubSmalltalkEditorTest >> source: anObject [
	source := anObject
]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidEmptySource [
	
	self source: ''.
	
	self 
		positionAfter: '';
		assertNodeError.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceNearNextMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	
	^self'.
	
	self 
		positionBefore: '^self' offset: -1;
		assertNodeValue: 300.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceNearPreviousMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionBefore: '1 +' offset: -2;
		assertNodeSelector: #msg1 description: 'should backup to closest node unaffected by error'.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceOnLineEnd [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '300';
		assertNodeValue: 300 description: 'should let you select a value you have just typed (e.g. to widen and bracket)'

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceOnMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+ description: 'should let you select an operator for implementorsOf etc.'.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodOnValidStatementBelowError [
	
	self source: 'testMethod
	self msg1.
		
	1 #broken
	
	self msg2.
	^self'.
	
	self 
		positionAfter: 'msg2';
		assertNodeSelector: #msg2 description: 'should let you select a valid selector in valid code after the error'

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundIncompleteMidSource [
	
	self source: '
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '300';
		assertNodeValue: 300.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundIncompleteMidSourceThenBrackets [
	
	self source: '
	self msg1.
		
	1 + 300
	(String new)'.
	
	self 
		positionAfter: 'String';
		assertNodeVariable: #String

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundParsableError [
	
	self source: 'fallbackBlock := [^self].
	node := self bestNodeInTextAreaOnError: fallbackBlock.
	
	node isMethod ifFalse: [ 
		node isValue and: [  node isSymbol ]
		[ node isMessage ] whileFalse: [ 
	 		(node := node parent) ifNil: fallbackBlock ]].'.
	
	self 
		positionAfter: 'isSymbol ]';
		assertPlaygroundNodeValue: 'RBBlockNode([ node isSymbol ])'.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundUnParsableError [
	
	self source: 'fallbackBlock := [^self].
	node := self bestNodeInTextAreaOnError: fallbackBlock.
	
	node isMethod ifFalse: [ 
		node isValue and: [  node isSymbol ]
		[ node isMessage ] whileFalse: [ false ]].'.
	
	self 
		positionAfter: 'isMessage ]';
		assertPlaygroundNodeValue: 'RBBlockNode([ node isMessage ])'.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidBinaryOperation [
	
	self source: 'testMethod
	5 + 100'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+.

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidCascadeLastStatement [

	self source: 'testMethod
	^self
		msg1;
		msg2;
		msg3'.
	
	self 
		positionAfter: 'msg3';
		assertNodeSelector: #msg3
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidCascadeMidStatement [

	self source: 'testMethod
	^self
		msg1;
		msg2;
		msg3'.
	
	self 
		positionAfter: 'msg2;';
		assertNodeSelector: #msg2
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidKeywordMessage [
	
	self source: 'testMethod
	1 to: 20 do: []'.
	
	self 
		positionAfter: 'to';
		assertNodeSelector: #to:do:

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPlayground [
	self source: '5 + 100'.
	
	self 
		positionAfter: '100';
		assertPlaygroundNodeValue: 100
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPlaygroundSimpleMsg [
	self source: 'self msg1
	'.
	
	self 
		positionAfter: 'msg1';
		assertPlaygroundNodeSelector: #msg1
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnMethodEnd [
	
	self source: 'testMethod
	5 + 9'.
	
	self 
		positionAfter: '9';
		assertNodeValue: 9

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnMethodPeriodEnd [

	self source: 'testMethod
	2 / 3.
	^5 + 9.'.
	
	self 
		positionAfter: '9.';
		assertNodeValue: 9
		
]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnReturningMethodEnd [

	self source: 'testMethod
	^5 + 9'.
	
	self 
		positionAfter: '9';
		assertNodeValue: 9
]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnStatementPeriodEnd [

	self source: 'testMethod
	2 / 3.
	^5 + 9.'.
	
	self 
		positionAfter: '3.';
		assertNodeValue: 3
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidSelectorMidSource [

	self source: 'testMethod
	5 + 3.
	7 / 8'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+
		

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidSimpleMethod [

	self source: 'testMethod
	^5 + 100
	'.
	
	self 
		positionAfter: '100';
		assertNodeValue: 100
	

]

{ #category : #tests }
RubSmalltalkEditorTest >> testBestNodeWithValidValueMidSource [

	self source: 'testMethod
	5 + 3.
	7 / 8'.
	
	self 
		positionAfter: '3';
		assertNodeValue: 3

]
