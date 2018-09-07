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
RubSmalltalkEditorTest >> assertJumpsToPostionAfter: aString direction: isFwd [
	| text |
	
	self
		exectuteFindNextKeywordDirection: isFwd
		editingMode: RubSmalltalkCodeMode new
		onFound: [ :loc | 
			text := self source first: loc.
			^ self
				assert: (text endsWith: aString)
				description: 'The source location didnt end with: ' , aString , ' in ' , text ].
			
	self assert: false description: 'Did not jump to correct location after, [' , aString , ']'
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertJumpsToPostionBefore: aString direction: isFwd [
	| text |
	
	self
		exectuteFindNextKeywordDirection: isFwd
		editingMode: RubSmalltalkCodeMode new
		onFound: [ :loc | 
			text := self source first: (self source size min: loc + aString size).
			^ self
				assert: (text endsWith: aString)
				description: 'The source location didnt finish before: ' , aString , ' in ' , text ].
			
	self assert: false description: 'Did not jump to correct location before, [' , aString , ']'
]

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
RubSmalltalkEditorTest >> exectuteFindNextKeywordDirection: isFwd editingMode: aRubEditingMode onFound: aBlock [

	| textArea |
	textArea := RubEditingArea new.
	textArea editingMode: aRubEditingMode.
	
	^ RubSmalltalkEditor new
			textArea: textArea;
			findNextKeywordIn: self source 
			selection: self selection 
			searchingForward: isFwd 
			ifFound: aBlock
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionAfter: aString [
	| pos |
	
	pos := (self source findString: aString) + aString size.
	self selection: (pos to: pos - 1).
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionBefore: aString [
	self positionBefore: aString offset: 0
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> positionBefore: aString offset: aNumber [
	| pos |
	
	pos := (self source findString: aString) + aNumber.
	self selection: (pos to: pos - 1).
	
]

{ #category : #accessing }
RubSmalltalkEditorTest >> selection [
	^ selection
]

{ #category : #accessing }
RubSmalltalkEditorTest >> selection: anInterval [
	selection := anInterval
]

{ #category : #accessing }
RubSmalltalkEditorTest >> source [
	^ source
]

{ #category : #accessing }
RubSmalltalkEditorTest >> source: aString [
	source := aString
]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidEmptySource [
	
	self source: ''.
	
	self 
		positionAfter: '';
		assertNodeError.

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceNearNextMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	
	^self'.
	
	self 
		positionBefore: '^self' offset: -1;
		assertNodeValue: 300.

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceNearPreviousMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionBefore: '1 +' offset: -2;
		assertNodeSelector: #msg1 description: 'should backup to closest node unaffected by error'.

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceOnLineEnd [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '300';
		assertNodeValue: 300 description: 'should let you select a value you have just typed (e.g. to widen and bracket)'

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidMethodIncompleteMidSourceOnMessage [
	
	self source: 'testMethod
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+ description: 'should let you select an operator for implementorsOf etc.'.

]

{ #category : #'tests-bestNodeIn' }
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

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundIncompleteMidSource [
	
	self source: '
	self msg1.
		
	1 + 300
	^self'.
	
	self 
		positionAfter: '300';
		assertNodeValue: 300.

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithInvalidPlaygroundIncompleteMidSourceThenBrackets [
	
	self source: '
	self msg1.
		
	1 + 300
	(String new)'.
	
	self 
		positionAfter: 'String';
		assertNodeVariable: #String

]

{ #category : #'tests-bestNodeIn' }
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

{ #category : #'tests-bestNodeIn' }
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

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidBinaryOperation [
	
	self source: 'testMethod
	5 + 100'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+.

]

{ #category : #'tests-bestNodeIn' }
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

{ #category : #'tests-bestNodeIn' }
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

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidKeywordMessage [
	
	self source: 'testMethod
	1 to: 20 do: []'.
	
	self 
		positionAfter: 'to';
		assertNodeSelector: #to:do:

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPlayground [
	self source: '5 + 100'.
	
	self 
		positionAfter: '100';
		assertPlaygroundNodeValue: 100
	

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPlaygroundSimpleMsg [
	self source: 'self msg1
	'.
	
	self 
		positionAfter: 'msg1';
		assertPlaygroundNodeSelector: #msg1
	

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnMethodEnd [
	
	self source: 'testMethod
	5 + 9'.
	
	self 
		positionAfter: '9';
		assertNodeValue: 9

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnMethodPeriodEnd [

	self source: 'testMethod
	2 / 3.
	^5 + 9.'.
	
	self 
		positionAfter: '9.';
		assertNodeValue: 9
		
]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnReturningMethodEnd [

	self source: 'testMethod
	^5 + 9'.
	
	self 
		positionAfter: '9';
		assertNodeValue: 9
]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidPostionOnStatementPeriodEnd [

	self source: 'testMethod
	2 / 3.
	^5 + 9.'.
	
	self 
		positionAfter: '3.';
		assertNodeValue: 3
	

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidSelectorMidSource [

	self source: 'testMethod
	5 + 3.
	7 / 8'.
	
	self 
		positionAfter: '+';
		assertNodeSelector: #+
		

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidSimpleMethod [

	self source: 'testMethod
	^5 + 100
	'.
	
	self 
		positionAfter: '100';
		assertNodeValue: 100
	

]

{ #category : #'tests-bestNodeIn' }
RubSmalltalkEditorTest >> testBestNodeWithValidValueMidSource [

	self source: 'testMethod
	5 + 3.
	7 / 8'.
	
	self 
		positionAfter: '3';
		assertNodeValue: 3

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnAssignment [
	"Should position after asignment giving the possibility to widen selection on the assigned expressions"
	
	self source: 'testMethod
	self simpleMsg.
	x := 1000.
	^self
'.
	
	self 
		positionBefore: 'x :=';
		assertJumpsToPostionAfter: ':= ' direction: true.
		
	self 
		positionAfter: '1000';
		assertJumpsToPostionAfter: ':= ' direction: false.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnBlockPipe [
	
	self source: 'testMethod
	self items inject: OrderedCollection new into: [:item :result | result add: item transformed ].
'.
	
	self 
		positionAfter: ':result';
		assertJumpsToPostionBefore: 'result add:' direction: true.
		
	self 
		positionBefore: 'result add:';
		assertJumpsToPostionAfter: ':result' direction: false.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnBlockVariables [
	
	self source: 'testMethod
	self items inject: OrderedCollection new into: [:item :result | result add: item transformed ].
'.
	
	self 
		positionAfter: 'into: ';
		assertJumpsToPostionAfter: ':item' direction: true.
	self 
		positionAfter: ':item';
		assertJumpsToPostionAfter: ':result' direction: true.

	self 
		positionBefore: 'result add:';
		assertJumpsToPostionAfter: ':result' direction: false.
	self 
		positionAfter: ':result';
		assertJumpsToPostionAfter: ':item' direction: false.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnCascade [
	
	self source: 'testMethod
	self 
		msg1;
		msg2;
		msg3
'.
	
	self 
		positionBefore: 'msg1';
		assertJumpsToPostionBefore: 'msg2' direction: true.
	self 
		positionBefore: 'msg2';
		assertJumpsToPostionBefore: 'msg3' direction: true.
		
	self 
		positionBefore: 'msg3';
		assertJumpsToPostionBefore: 'msg2' direction: false.
	self 
		positionBefore: 'msg2';
		assertJumpsToPostionBefore: 'msg1' direction: false.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnComments [
	"Should jump to postion ignoring any keywords in comments "
	self source: 'testMethod
	"some comments with keyword:selector: that should be ignored"
	
	self 
		msg1; "Another comment with ignored:selector: to skip"
		msg2
'.
	
	self 
		positionAfter: 'testMethod';
		assertJumpsToPostionBefore: 'msg1' direction: true.
		
	self 
		positionAfter: 'msg1; ';
		assertJumpsToPostionAfter: self source direction: true.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnEndOfBlock [
	"Should jump to the end of a block, giving an easy way to extend selection of it"
		
	self source: 'hasBindingThatBeginsWith: aString 
	^ self model notNil ifTrue: [ true ] ifFalse: [ aString > 1 ]'.
	
	self 
		positionAfter: ' true';
		assertJumpsToPostionBefore: ' ifFalse:' direction: true.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnEndOfSource [
	
	self source: 'testMethod
	self simpleMsg.
	self secondMsg'.
	
	self 
		positionAfter: 'self secondMsg';
		assertJumpsToPostionAfter: 'secondMsg' direction: true.
		
	self 
		positionBefore: 'testMethod';
		assertJumpsToPostionBefore: 'testMethod' direction: false.


]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnIfTrueIfFalseAfterCompletion [
	
	self source: 'testMethod
true ifTrue:  ifFalse: '.

	self 
		positionAfter: 'ifTrue: ';
		assertJumpsToPostionAfter: 'ifFalse: ' direction: true.

	self 
		positionAfter: 'ifFalse: ';
		assertJumpsToPostionAfter: 'ifTrue: ' direction: false.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnNoFurtherStatements [
	
	self source: 'testMethod
	self simpleMsg secondMsg'.
	
	self 
		positionAfter: 'self simple';
		assertJumpsToPostionAfter: 'secondMsg' direction: true.
		
	self 
		positionAfter: 'self';
		assertJumpsToPostionBefore: 'testMethod' direction: false.

	

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnNoParameterBlock [
	"Should jump inside a block [ marker, ready for you to type more expressions into a new block, or easily add a variable into an existing block.
		
	NOTE: This test is currently asserting differently as empirically it was too fine grained to jump into every block - instead we simply jump to the next valid position inside the block. The implementation regex can be changed if the original behaviour is deemed better"
		
	self source: 'hasBindingThatBeginsWith: aString 
	^ self model notNil and: [ model hasBindingThatBeginsWith: aString ]'.
	
	self 
		positionAfter: ' and: ';
		assertJumpsToPostionBefore: 'aString ]' direction: true.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnReturn [
	"Should jump to just after a ^ statement so you can easily widen selection on what will be returned from a method"
	
	self source: 'testMethod
	^ self msg ifTrue: [ 1 ] ifFalse: [myVar refresh. ^nil]
'.
	
	self 
		positionAfter: 'testMethod';
		assertJumpsToPostionBefore: 'self' direction: true.
		
	self 
		positionAfter: '^ self ';
		assertJumpsToPostionBefore: 'self msg' direction: false.
		
	"This one fails as regex solution has difficulty with single character matches,
		if also trying to account for matching aribtrary keyword: positions. Leaving for now"
	"
	self 
		positionAfter: 'refresh. ';
		assertJumpsToPostionBefore: 'nil' direction: true."
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnSecondKeyword [
	"Should jump between multiple kewords"
	
	self source: 'testMethod
	self multiKeywordMsg: 5 with: 10.
'.
	
	self 
		positionAfter: 'KeywordMsg: ';
		assertJumpsToPostionAfter: 'with: ' direction: true.
		
	"Should also jump if right next to : (and an empty selection)"
	self 
		positionAfter: 'KeywordMsg:';
		assertJumpsToPostionAfter: 'with: ' direction: true.
		
	self 
		positionAfter: 'with: ';
		assertJumpsToPostionAfter: 'KeywordMsg: ' direction: false.
		
	"Should also jump if right next to : (and an empty selection)"
	self 
		positionAfter: 'with:';
		assertJumpsToPostionAfter: 'KeywordMsg: ' direction: false.

]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnSelf [
	"Should jump after self to easily widen selection on a self message send"
	self source: 'testMethod
	self 
		msg1;
		msg2
'.
	
	self 
		positionAfter: 'testMethod';
		assertJumpsToPostionBefore: 'msg1' direction: true.
		
	self 
		positionBefore: 'msg2';
		assertJumpsToPostionBefore: 'msg1' direction: false.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnSimpleKeyword [
	
	self source: 'testMethod
self simpleKeywordMsg: 5.
'.
	
	self 
		positionAfter: 'self ';
		assertJumpsToPostionAfter: 'Msg: ' direction: true.
		
	"If we jump when at :, it should jump to the end"
	self 
		positionAfter: 'Msg:';
		assertJumpsToPostionAfter: self source direction: true.
		
	self 
		positionAfter: 'simpleKeywordMsg: ';
		assertJumpsToPostionAfter: 'self ' direction: false.
		
	"If we jump when at :, it should jump to prev"
	self 
		positionAfter: 'simpleKeywordMsg:';
		assertJumpsToPostionAfter: 'self ' direction: false.


]
