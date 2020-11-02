Class {
	#name : #RubSendersOfSelectionTest,
	#superclass : #TestCase,
	#instVars : [
		'editor',
		'editingArea',
		'sourceCode',
		'ast'
	],
	#category : #'Rubric-Tests-Base'
}

{ #category : #helpers }
RubSendersOfSelectionTest >> caretAfter: aNode [

	self caretAt: aNode stop + 1
]

{ #category : #helpers }
RubSendersOfSelectionTest >> caretAt: aPosition [ 

	editingArea selectFrom: aPosition to: aPosition - 1
]

{ #category : #helpers }
RubSendersOfSelectionTest >> caretBefore: aNode [ 

	self caretAt: aNode start
]

{ #category : #helpers }
RubSendersOfSelectionTest >> caretInMiddleOf: aNode [ 
	
	self caretAt: aNode start + 1
]

{ #category : #helpers }
RubSendersOfSelectionTest >> caretInMiddleOfCascadeMessage: aRBMessageNode [ 
	
	self flag: #bug.
	"The start of a selector node should not be an integer array. That will change in the future and this test will break because of that."
	^ self caretAt: aRBMessageNode selectorNode start first
]

{ #category : #helpers }
RubSendersOfSelectionTest >> messageNodeToTest [

	^ self returnNodeToTest value
]

{ #category : #running }
RubSendersOfSelectionTest >> method: a zkx: bsq pn: c [

	^ self aMethod: a with: (self something: bsq) x:(self perform: #selector)
]

{ #category : #running }
RubSendersOfSelectionTest >> methodWithCascade [

	^ self
		yourself;
		asString
]

{ #category : #running }
RubSendersOfSelectionTest >> methodWithIfTrueIfFalse [

	self isInteger
		ifTrue: [ 
			#(1 2) first.
			self printString ]
		ifFalse: [ self yourself ]
]

{ #category : #running }
RubSendersOfSelectionTest >> methodX:a [

	
]

{ #category : #helpers }
RubSendersOfSelectionTest >> open [

	editingArea openInWindow
]

{ #category : #helpers }
RubSendersOfSelectionTest >> returnNodeToTest [

	^ ast statements first
]

{ #category : #helpers }
RubSendersOfSelectionTest >> select: someText [ 
	
	sourceCode := someText.
	editingArea updateTextWith: sourceCode asText.
	
	editingArea selectAll
]

{ #category : #helpers }
RubSendersOfSelectionTest >> selectedSelector [

	| extractor |
	extractor := RubSelectionExtractor new.
	^ extractor extractSelectedSelectorFrom: editor
]

{ #category : #running }
RubSendersOfSelectionTest >> setUp [ 

	super setUp.
	
	editingArea := 	RubEditingArea new
		beForSmalltalkCode;
		beWrapped;
		width: 400;
		yourself.
		
	editor := RubSmalltalkEditor forTextArea: editingArea.
]

{ #category : #helpers }
RubSendersOfSelectionTest >> sourceCodeOf: aMethod [ 
	
	sourceCode := aMethod sourceCode.
	ast := aMethod ast.
	editingArea updateTextWith: sourceCode asText
]

{ #category : #'tests - no selection - blocks' }
RubSendersOfSelectionTest >> testCaretAfterFirstMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretAfter: messageInsideBlock.
	
	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : #'tests - no selection - blocks' }
RubSendersOfSelectionTest >> testCaretAfterLastMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements last.
	self caretAfter: messageInsideBlock.
	
	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : #'tests - no selection - blocks' }
RubSendersOfSelectionTest >> testCaretAfterMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretAfter: messageInsideBlock.
	
	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretAfterNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	
	"The argument is a message node, we should match it instead of the parent"
	self caretAfter: self messageNodeToTest receiver.
	
	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : #'tests - no selection - return' }
RubSendersOfSelectionTest >> testCaretAfterReturnShouldMatchChild [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	
	"Caret in middle of should put the caret after the ^"
	self caretInMiddleOf: self returnNodeToTest.
	
	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : #'tests - no selection - return' }
RubSendersOfSelectionTest >> testCaretAfterReturnShouldMatchChildCascade [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.
	
	"Caret in middle of should put the caret after the ^"
	self caretInMiddleOf: self returnNodeToTest.
	
	cascade := self returnNodeToTest value.
	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : #'tests - no selection - blocks' }
RubSendersOfSelectionTest >> testCaretBeforeMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretBefore: messageInsideBlock.
	
	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretBeforeNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	
	"The argument is a message node, we should match it instead of the parent"
	self caretBefore: self messageNodeToTest receiver.
	
	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : #'tests - no selection - return' }
RubSendersOfSelectionTest >> testCaretBeforeReturnShouldMatchChild [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: self returnNodeToTest.
	
	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : #'tests - no selection - cascade' }
RubSendersOfSelectionTest >> testCaretInCascadeFirstMessageShouldMatchFirstMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.
	
	cascade := ast statements first value.
	self caretInMiddleOfCascadeMessage: cascade messages first.
	
	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : #'tests - no selection - cascade' }
RubSendersOfSelectionTest >> testCaretInCascadeReceiverShouldMatchFirstMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.
	
	cascade := ast statements first value.
	self caretInMiddleOf: cascade receiver.
	
	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : #'tests - no selection - cascade' }
RubSendersOfSelectionTest >> testCaretInCascadeSecondMessageShouldMatchSecondMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.
	
	cascade := ast statements first value.
	self caretInMiddleOfCascadeMessage: cascade messages second.
	
	self assert: self selectedSelector equals: cascade messages second selector
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderAfterArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretAfter: (ast arguments at: 1).
	
	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderInBeforeArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: (ast arguments at: 1).
	
	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderInBeforeArgumentWithoutSpaceName [

	self sourceCodeOf: self class >> #methodX:.
	self caretBefore: (ast arguments at: 1).
	
	self assert: self selectedSelector equals: #methodX:
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: (ast arguments at: 2).
	
	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderInMiddleOfMessageSelector [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: self messageNodeToTest.

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : #'tests - no selection - method signature' }
RubSendersOfSelectionTest >> testCaretInHeaderInMiddleOfSelectorHeader [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: ast.
	
	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInMessageAfterArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretAfter: (self messageNodeToTest arguments at: 1).
	
	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInMessageInBeforeArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: (self messageNodeToTest arguments at: 1).
	
	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : #'tests - no selection - blocks' }
RubSendersOfSelectionTest >> testCaretInMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretInMiddleOf: messageInsideBlock.
	
	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInNestedMessageInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	
	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: (self messageNodeToTest arguments at: 2).
	
	self assert: self selectedSelector equals: self messageNodeToTest arguments second selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInNestedPerformMessageInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	
	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: self messageNodeToTest arguments third.
	
	self assert: self selectedSelector equals: self messageNodeToTest arguments third selector
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInSymbol [

	| performSymbolArgumentNode |
	self sourceCodeOf: self class >> #method:zkx:pn:.
	
	"The argument is a message node, we should match it instead of the parent"
	performSymbolArgumentNode := self messageNodeToTest arguments third arguments first.
	self caretInMiddleOf: performSymbolArgumentNode.
	
	self assert: self selectedSelector equals: performSymbolArgumentNode value
]

{ #category : #'tests - no selection - message' }
RubSendersOfSelectionTest >> testCaretInTheMiddleOfNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	
	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: self messageNodeToTest receiver.
	
	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectCascadeFirstMessageShouldMatchFirstMessageSelector [

	self select: 'foo bar: 7; fum'.
	
	self assert: self selectedSelector equals: #bar:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectCascadeFirstMessageWithoutReceiverShouldMatchFirstMessageSelector [

	self select: 'bar: 7; fum'.

	self assert: self selectedSelector equals: #bar:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectKeywordMessageShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: #toto) fum: [ :will | it break ]'.
	
	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectKeywordMessageWithBrokenLastArgumentShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: #toto) fum: [ :will | it b'.
	
	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectKeywordMessageWithBrokenMiddleArgumentShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: # fum: [ :will | it break ]'.
	
	self assert: self selectedSelector equals: #foo:bar:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectKeywordMessageWithoutReceiverAndComplexArgumentsShouldMatchKeywordSelector [

	self select: 'foo: 17 bar: (some message: #toto) fum: [ :will | it break ]'.

	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectKeywordMessageWithoutReceiverShouldMatchKeywordSelector [

	self select: 'method: a zkx: bsq pn: c'.

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectMultipleStatementsShouldMatchFirstStatement [

	self select: 'foo. second statement'.
	
	self assert: self selectedSelector equals: #foo
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectReturnShouldMatchToken [

	self select: '^ foo'.
	
	self assert: self selectedSelector equals: #foo
]

{ #category : #'tests - selection' }
RubSendersOfSelectionTest >> testSelectTokenShouldMatchToken [

	self select: 'foo'.
	
	self assert: self selectedSelector equals: #foo
]
