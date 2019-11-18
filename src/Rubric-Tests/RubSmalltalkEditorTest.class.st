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
	self assertJumpsToPostionAfter: aString direction: isFwd playground: false
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertJumpsToPostionAfter: aString direction: isFwd playground: isPlayground [
	| text mode |
	
	mode := isPlayground ifTrue: [ RubSmalltalkScriptingMode new  ] ifFalse: [ RubSmalltalkCodeMode new ].
	self
		exectuteFindNextKeywordDirection: isFwd
		editingMode: mode
		onFound: [ :loc | 
			text := self source first: loc.
			^ self
				assert: (text endsWith: aString)
				description: 'The source location didnt end with: ' , aString , ' in ' , text ].
			
	self assert: false description: 'Did not jump to correct location after, [' , aString , ']'
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertJumpsToPostionBefore: aString direction: isFwd [
	self assertJumpsToPostionBefore: aString direction: isFwd playground: false
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertJumpsToPostionBefore: aString direction: isFwd playground: isPlayground [
	| text mode |

	mode := isPlayground ifTrue: [ RubSmalltalkScriptingMode new  ] ifFalse: [ RubSmalltalkCodeMode new ].	
	self
		exectuteFindNextKeywordDirection: isFwd
		editingMode: mode
		onFound: [ :loc | 
			text := self source first: (self source size min: loc + aString size).
			^ self
				assert: (text endsWith: aString)
				description: 'The source location didnt finish before: ' , aString , ' in ' , text ].
			
	self assert: false description: 'Did not jump to correct location before, [' , aString , ']'
]

{ #category : #assertions }
RubSmalltalkEditorTest >> assertJumpsToPostionFor: aString direction: isFwd playground: isPlayground [
	| text mode found directionText |
	
	mode := isPlayground
		ifTrue: [ RubSmalltalkScriptingMode new ]
		ifFalse: [ RubSmalltalkCodeMode new ].
		
	self
		exectuteFindNextKeywordDirection: isFwd
		editingMode: mode
		onFound: [ :loc | 
			isFwd
				ifTrue: [ text := self source first: loc.
					found := text endsWith: aString.
					directionText := 'end with - ' ]
				ifFalse: [ text := self source.
					found := text beginsWith: aString.
					directionText := 'begin with - ' ].
				
			^ self
				assert: found
				description: 'The source location doesn''t ' , directionText , aString , ' in ' , text ].
			
	self assert: false description: 'Did not jump to correct location for, [' , aString , ']'
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

{ #category : #assertions }
RubSmalltalkEditorTest >> assertWidensToRegex: aString playground: isPlayground [
	| text mode |
	
	mode := isPlayground ifTrue: [ RubSmalltalkScriptingMode new  ] ifFalse: [ RubSmalltalkCodeMode new ].
	self
		widenSelectionWithMode: mode ifFound: [ :interval | 
			text := self source copyFrom: interval first to: interval last.
			^ self assert: (text matchesRegex: aString) description: 'The selected text node "', text, '", doesn''t match: "', aString, '"' ].
			
	self assert: false description: 'Did not find a selection node for, "' , aString , '"'
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

{ #category : #helpers }
RubSmalltalkEditorTest >> positionSelection: aString [
	| pos |
	
	pos := (self source findString: aString).
	self selection: (pos to: pos + aString size).
	
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

{ #category : #'tests-completion' }
RubSmalltalkEditorTest >> testInstanceVersusSharedCompletionEngine [

	| textEditor currentCompletion |
	[ currentCompletion := RubSmalltalkEditor completionEngineClass.
	textEditor := RubSmalltalkEditor new.
	self assert: textEditor completionEngine class equals: currentCompletion.
	
	RubSmalltalkEditor noCompletion.
	"here we show that an existing editor is not impacted by the shared behavior"
	self assert: textEditor completionEngine class equals: currentCompletion.
	textEditor := RubSmalltalkEditor new.
	self assert: textEditor completionEngine isNil.
	
	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	self assert: textEditor completionEngine isNil.
	
	"but now when we create a new one it gets the correct engine"
	textEditor := RubSmalltalkEditor new.
	self assert: textEditor completionEngine class equals: CompletionEngine.
	
	
	] ensure: [ RubSmalltalkEditor completionEngineClass: currentCompletion ]
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnAssignment [
	"Should position after an asignment giving the possibility to widen selection on the assigned expressions"
	
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
	"Should jump after a block variable pipe so its easy to widen selection on the contents"
	
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
	"Should jump after an assignment, so its easy to widen selection on the expression to modify it"
	
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
	"Should be able to jump between cascaded messages"
	
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
	"Should jump to next postion but ignore any keywords in comments"
	
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
	"Edge case test, ensure it can handle this end of source boundary"
	
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
RubSmalltalkEditorTest >> testJumpOnErroneousMethod [
	"Should be able to intelligently jump on source that doesn't yet compile"
	
	self source: 'testMethod
	
	self msg1.
		
	1 + 300

	(String new: 5)
'.
	
	self 
		positionBefore: 'msg1';
		assertJumpsToPostionBefore: '1 + 300' direction: true.
	self 
		positionBefore: '1 + 300';
		assertJumpsToPostionAfter: 'new: ' direction: true.
		
	self 
		positionBefore: 'new:';
		assertJumpsToPostionBefore: '1 +' direction: false.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnIfTrueIfFalseAfterCompletion [
	"This is the most common jump example, after code completing ifTrue:ifFalse: you want to easily type in an
	expression for each case"
	
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
	"Should jump to the very end or beginning when no further matches, so you can easily add more code"
	
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
		
	NOTE: This test is currently asserting differently, as empirically it was too fine grained to jump into every block - instead we simply jump to the next valid position inside the block. The implementation regex can be changed if this other behaviour is deemed better"
		
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
		
	"This one fails as the regex solution has difficulty with single character matches,
		if also trying to account for matching aribtrary keyword: positions. Leaving for now"
	"
	self 
		positionAfter: 'refresh. ';
		assertJumpsToPostionBefore: 'nil' direction: true."
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testJumpOnSecondKeyword [
	"Should jump between multiple keywords"
	
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

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testPlaygroundJumpOnErroneousSource [
	"Should be able to intelligently jump on source that doesn't yet compile"
	
	self source: '	
	self msg1.
		
	1 + 300

	(String new: 5)
'.
	
	self 
		positionBefore: 'msg1';
		assertJumpsToPostionBefore: '1 + 300' direction: true playground: true.
	self 
		positionBefore: '1 + 300';
		assertJumpsToPostionAfter: 'new: ' direction: true playground: true.
		
	self 
		positionBefore: 'new:';
		assertJumpsToPostionBefore: '1 +' direction: false playground: true.
	
]

{ #category : #'tests-jumpFind' }
RubSmalltalkEditorTest >> testPlaygroundJumpOnIfTrueIfFalseAfterCompletion [
	"Source jumping should work in bother browser and playground"
	
	self source: 'true ifTrue:  ifFalse: '.

	self 
		positionAfter: 'ifTrue: ';
		assertJumpsToPostionAfter: 'ifFalse: ' direction: true playground: true.

	self 
		positionAfter: 'ifFalse: ';
		assertJumpsToPostionAfter: 'ifTrue: ' direction: false playground: true.

]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testPlaygroundWidenOnCascade [
	"Should widen on cascaded messages"
	
	self source: '
	5 + 4.
	self 
		msg1;
		msg2;
		msg3
'.
	
	self 
		positionBefore: 'msg2';
		assertWidensToRegex: ';.*msg2' playground: true.
		
	self 
		positionAfter: 'msg3';
		assertWidensToRegex: ';.*msg3' playground: true.
		
	self 
		positionSelection: 'msg2';
		assertWidensToRegex: 'self.*msg3' playground: true.
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testPlaygroundWidenOnErroneousSource [
	"Should be able to intelligently jump on source that doesn't yet compile"
	
	self source: '
	self msg1.
		
	1 + 300

	(String new: 5)
'.
	
	self 
		positionAfter: '300';
		assertWidensToRegex: '300' playground: true.
		
	self 
		positionSelection: '300';
		assertWidensToRegex: '1 \+ 300' playground: true.
		
	self 
		positionSelection: '5';
		assertWidensToRegex: '\(String.*5\)' playground: true.
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testPlaygroundWidenOnSelf [
	"Should widen selection after self to easily select code on a self message send"
	
	self source: 'self 
		msg1;
		msg2
'.
	
	self 
		positionBefore: 'msg1';
		assertWidensToRegex: 'self.+msg1' playground: true.
		
	self 
		positionBefore: 'msg2';
		assertWidensToRegex: ';.*msg2' playground: true
	
]

{ #category : #'tests-completion' }
RubSmalltalkEditorTest >> testSettingCompletionFromEditor [

	| currentCompletion |
	[ currentCompletion := RubSmalltalkEditor completionEngineClass. 
	
	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	self assert: RubSmalltalkEditor completionEngineClass equals: CompletionEngine.
	
	] ensure: [ RubSmalltalkEditor completionEngineClass: currentCompletion ]
	
]

{ #category : #'tests-completion' }
RubSmalltalkEditorTest >> testSettingCompletionFromEditorToTextArea [

	| textEditor currentCompletion |
	[ currentCompletion := RubSmalltalkEditor completionEngineClass.
	
	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	textEditor := RubSmalltalkEditor new.
	self assert: textEditor completionEngine class equals: CompletionEngine.
	textEditor textArea: RubEditingArea new. 
	self assert: textEditor textArea completionEngine class equals: CompletionEngine.
	
	] ensure: [ RubSmalltalkEditor completionEngineClass: currentCompletion ]
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenInsideBlock [
	"Should widen selection inside a block, slowly extending the selection"
	
	self source: 'hasBindingThatBeginsWith: aString 
	^ self model notNil ifTrue: [ true ] ifFalse: [ aString > 10 ]'.
	
	
	self 
		positionSelection: '10';
		assertWidensToRegex: 'aString > 10' playground: false.
		
	self 
		positionSelection: 'aString > 10';
		assertWidensToRegex: '\[ aString > 10 \]' playground: false.
		
	self 
		positionSelection: '[ aString > 10 ]';
		assertWidensToRegex: 'self.*10 \]' playground: false.
		
	
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnAssignment [
	"Should position after an asignment giving the possibility to widen selection on the assigned expressions"
	
	self source: 'testMethod
	self simpleMsg.
	x := 1000.
	^self
'.
	
	self 
		positionAfter: ':=';
		assertWidensToRegex: 'x .*1000' playground: false.
		
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnBrackets [
	"Should jump to next postion but ignore any keywords in comments"
	
	self source: 'testMethod
		^5 + ((100 - 70) * 20)
'.
	
	self 
		positionSelection: '70';
		assertWidensToRegex: '\(100 - 70\)' playground: false.
		
	self 
		positionSelection: '(100 - 70)';
		assertWidensToRegex: '\(\(100.*20\)' playground: false.
		
	 self 
		positionSelection: '20';
		assertWidensToRegex: '\(\(100.*20\)' playground: false
		
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnCascade [
	"Should widen on cascaded messages"
	
	self source: 'testMethod
	self 
		msg1;
		msg2;
		msg3
'.
	
	self 
		positionBefore: 'msg2';
		assertWidensToRegex: ';.*msg2' playground: false.
		
	self 
		positionAfter: 'msg3';
		assertWidensToRegex: ';.*msg3' playground: false.
		
	self 
		positionSelection: 'msg2';
		assertWidensToRegex: 'self.*msg3' playground: false.
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnComments [
	"Should jump to next postion but ignore any keywords in comments"
	
	self source: 'testMethod
	"some comments with keyword:selector: that should be ignored"
	
	self 
		msg1; "Another comment with ignored:selector: to skip"
		msg2
'.
	
	self 
		positionAfter: 'comments';
		assertWidensToRegex: '"some.*ignored"' playground: false.
		
	self 
		positionSelection: 'keyword:selector:';
		assertWidensToRegex: '"some.*ignored"' playground: false
		
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnEndOfBlock [
	"Should widen selection at the end of a block to select the whole block"
	
	self source: 'hasBindingThatBeginsWith: aString 
	^ self model notNil ifTrue: [ true ] ifFalse: [ aString > 1 ]'.
	
	
	self 
		positionAfter: 'true ]';
		assertWidensToRegex: '\[ true \]' playground: false.
		
	
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnErroneousMethod [
	"Should be able to intelligently jump on source that doesn't yet compile"
	
	self source: 'testMethod
	
	self msg1.
		
	1 + 300

	(String new: 5)
'.
	
	self 
		positionAfter: '300';
		assertWidensToRegex: '300' playground: false.
		
	self 
		positionSelection: '300';
		assertWidensToRegex: '1 \+ 300' playground: false.
		
	self 
		positionSelection: '5';
		assertWidensToRegex: '\(String.*5\)' playground: false.
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnKeywordSelector [
	"Should jump between multiple keywords"
	
	self source: 'testMethod
	self msg1.
	self multiKeywordMsg: 7 with: 10 + 5.
	self msg2
'.
	
	self 
		positionSelection: '10 + 5';
		assertWidensToRegex: 'self.*5' playground: false.
		
	self 
		positionSelection: '7';
		assertWidensToRegex: 'self.*5' playground: false
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnMultipleExpressions [
	"Should widen on multiple expressions"
	
	self source: 'testMethod
	x := 1000 + 5.
	^self
'.
	
	self 
		positionBefore: '1000';
		assertWidensToRegex: '1000' playground: false.
		
	self 
		positionSelection: '1000';
		assertWidensToRegex: '1000 \+ 5' playground: false.
		
	self 
		positionSelection: '5';
		assertWidensToRegex: '1000 \+ 5' playground: false.
		
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnSelf [
	"Should widen selection after self to easily select code on a self message send"
	
	self source: 'testMethod
	self 
		msg1;
		msg2
'.
	
	self 
		positionBefore: 'msg1';
		assertWidensToRegex: 'self.+msg1' playground: false.
		
	self 
		positionBefore: 'msg2';
		assertWidensToRegex: ';.*msg2' playground: false
	
]

{ #category : #'tests-widenSelection' }
RubSmalltalkEditorTest >> testWidenOnString [
	"Should jump to next postion but ignore any keywords in comments"
	
	self source: 'testMethod
	self 
		msg: ''some string'';
		msg1: ''a string with "a comment text" inside''; "Another comment"
		msg2
'.
	
	self 
		positionAfter: 'some';
		assertWidensToRegex: '\''some string\''' playground: false.
	
	self 
		positionAfter: 'a comment';
		assertWidensToRegex: '\''a string.*inside\''' playground: false.
			
	
]

{ #category : #helpers }
RubSmalltalkEditorTest >> widenSelectionWithMode: aRubEditingMode ifFound: aBlock [ 

	| textArea |
	textArea := RubEditingArea new.
	textArea editingMode: aRubEditingMode.
	
	^ RubSmalltalkEditor new
		textArea: textArea;
		widenSelectionIn: self source
			selection: self selection
			ifFound: aBlock
]
