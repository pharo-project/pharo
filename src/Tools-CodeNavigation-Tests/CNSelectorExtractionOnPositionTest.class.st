Class {
	#name : 'CNSelectorExtractionOnPositionTest',
	#superclass : 'TestCase',
	#instVars : [
		'sourceCode',
		'ast',
		'position'
	],
	#category : 'Tools-CodeNavigation-Tests',
	#package : 'Tools-CodeNavigation-Tests'
}

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> caretAfter: aNode [

	self caretAt: aNode stop + 1
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> caretAt: aPosition [

	position := aPosition
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> caretBefore: aNode [

	self caretAt: aNode start
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> caretInMiddleOf: aNode [

	self caretAt: aNode start + 1
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> caretInMiddleOfCascadeMessage: aMessageNode [

	self flag: #bug.
	"The start of a selector node should not be an integer array. That will change in the future and this test will break because of that."
	^ self caretAt: aMessageNode selectorNode start
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> messageNodeToTest [

	^ self returnNodeToTest value
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> method: a zkx: bsq pn: c [
	<ignoreNotImplementedSelectors: #(aMethod:with:x: something:)>
	^ self aMethod: a with: (self something: bsq) x:(self perform: #selector)
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> methodWithCascade [

	^ self
		yourself;
		asString
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> methodWithIfTrueIfFalse [

	self isInteger
		ifTrue: [
			#(1 2) first.
			self printString ]
		ifFalse: [ self yourself ]
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> methodWithSpacesAtTheEnd [
	"Please do not remove the leading spaces"
	<ignoreNotImplementedSelectors: #(someMessageFollowedBySpaces)>
	^ 1 someMessageFollowedBySpaces  
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> methodX:a [
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> returnNodeToTest [

	^ ast statements first
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> selectedSelector [

	^ CNSelectorExtractor new extractSelectorFromAST: ast atPosition: position
]

{ #category : 'helpers' }
CNSelectorExtractionOnPositionTest >> sourceCodeOf: aMethod [

	sourceCode := aMethod sourceCode.
	ast := aMethod ast
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterABlock [
	| selector |
	selector := '[]'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterABlockWithArgument [
	| selector |
	selector := '[:even| ]'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterADynamicArray [
	| selector |
	selector := '{}'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterADynamicArrayWithAnElement [
	| selector |
	selector := '{even}'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterALiteralArray [
	| selector |
	selector := '#()'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretAfterALiteralArrayWithAnElement [
	| selector |
	selector := '#(even)'.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testCaretAfterFirstMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretAfter: messageInsideBlock.

	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testCaretAfterLastMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements last.
	self caretAfter: messageInsideBlock.

	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testCaretAfterMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretAfter: messageInsideBlock.

	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretAfterNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.

	"The argument is a message node, we should match it instead of the parent"
	self caretAfter: self messageNodeToTest receiver.

	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : 'tests - no selection - return' }
CNSelectorExtractionOnPositionTest >> testCaretAfterReturnShouldMatchChild [

	self sourceCodeOf: self class >> #method:zkx:pn:.

	"Caret in middle of should put the caret after the ^"
	self caretInMiddleOf: self returnNodeToTest.

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : 'tests - no selection - return' }
CNSelectorExtractionOnPositionTest >> testCaretAfterReturnShouldMatchChildCascade [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.

	"Caret in middle of should put the caret after the ^"
	self caretInMiddleOf: self returnNodeToTest.

	cascade := self returnNodeToTest value.
	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : 'tests - no selection - variable' }
CNSelectorExtractionOnPositionTest >> testCaretAfterVariableNode [

	| selector |
	selector := #even.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: selector
]

{ #category : 'tests - no selection - return' }
CNSelectorExtractionOnPositionTest >> testCaretAtEndOfMethodShouldMatchMethodNode [

	self sourceCodeOf: self class >> #methodWithSpacesAtTheEnd.

	self caretAfter: self returnNodeToTest methodNode.

	self assert: self selectedSelector equals: #methodWithSpacesAtTheEnd
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testCaretBeforeMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretBefore: messageInsideBlock.

	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretBeforeNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.

	"The argument is a message node, we should match it instead of the parent"
	self caretBefore: self messageNodeToTest receiver.

	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : 'tests - no selection - return' }
CNSelectorExtractionOnPositionTest >> testCaretBeforeReturnShouldMatchChild [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: self returnNodeToTest.

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : 'tests - no selection - cascade' }
CNSelectorExtractionOnPositionTest >> testCaretInCascadeFirstMessageShouldMatchFirstMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.

	cascade := ast statements first value.
	self caretInMiddleOfCascadeMessage: cascade messages first.

	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : 'tests - no selection - cascade' }
CNSelectorExtractionOnPositionTest >> testCaretInCascadeReceiverShouldMatchFirstMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.

	cascade := ast statements first value.
	self caretInMiddleOf: cascade receiver.

	self assert: self selectedSelector equals: cascade messages first selector
]

{ #category : 'tests - no selection - cascade' }
CNSelectorExtractionOnPositionTest >> testCaretInCascadeSecondMessageShouldMatchSecondMessage [

	| cascade |
	self sourceCodeOf: self class >> #methodWithCascade.

	cascade := ast statements first value.
	self caretInMiddleOfCascadeMessage: cascade messages second.

	self assert: self selectedSelector equals: cascade messages second selector
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderAfterArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretAfter: (ast arguments at: 1).

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderInBeforeArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: (ast arguments at: 1).

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderInBeforeArgumentWithoutSpaceName [

	self sourceCodeOf: self class >> #methodX:.
	self caretBefore: (ast arguments at: 1).

	self assert: self selectedSelector equals: #methodX:
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: (ast arguments at: 2).

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderInMiddleOfMessageSelector [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: self messageNodeToTest.

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : 'tests - no selection - method signature' }
CNSelectorExtractionOnPositionTest >> testCaretInHeaderInMiddleOfSelectorHeader [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretInMiddleOf: ast.

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInMessageAfterArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretAfter: (self messageNodeToTest arguments at: 1).

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInMessageInBeforeArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.
	self caretBefore: (self messageNodeToTest arguments at: 1).

	self assert: self selectedSelector equals: self messageNodeToTest selector
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testCaretInMessageInBlock [

	| firstBlock messageInsideBlock |
	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	firstBlock := ast statements first arguments first.
	messageInsideBlock := firstBlock statements first.
	self caretInMiddleOf: messageInsideBlock.

	self assert: self selectedSelector equals: messageInsideBlock selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInNestedMessageInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.

	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: (self messageNodeToTest arguments at: 2).

	self assert: self selectedSelector equals: self messageNodeToTest arguments second selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInNestedPerformMessageInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #method:zkx:pn:.

	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: self messageNodeToTest arguments third.

	self assert: self selectedSelector equals: self messageNodeToTest arguments third selector
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInSymbol [

	| performSymbolArgumentNode |
	self sourceCodeOf: self class >> #method:zkx:pn:.

	"The argument is a message node, we should match it instead of the parent"
	performSymbolArgumentNode := self messageNodeToTest arguments third arguments first.
	self caretInMiddleOf: performSymbolArgumentNode.

	self assert: self selectedSelector equals: performSymbolArgumentNode value
]

{ #category : 'tests - no selection - message' }
CNSelectorExtractionOnPositionTest >> testCaretInTheMiddleOfNestedMessageReceiverInMiddleOfArgumentName [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.

	"The argument is a message node, we should match it instead of the parent"
	self caretInMiddleOf: self messageNodeToTest receiver.

	self assert: self selectedSelector equals: self messageNodeToTest receiver selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretOnNonValidCode [
	"Code has invalid code (no selectors at all)"


	sourceCode := 'anObjectdo : 2'.
	ast := (OCParser parseFaultyExpression: sourceCode).
	self caretAfter: ast.
	self assert: self selectedSelector equals: ast methodNode selector
]

{ #category : 'tests - no selection - withOutMessage' }
CNSelectorExtractionOnPositionTest >> testCaretWithEmptyString [
	| selector |
	selector := ''.
	sourceCode := selector.
	ast := (OCParser parseFaultyExpression: selector).
	self caretAfter: ast.
	self assert: self selectedSelector equals: nil
]

{ #category : 'tests - no selection - blocks' }
CNSelectorExtractionOnPositionTest >> testExtractSelectorWithPositionAtFirstCharacterOfMethod [

	self sourceCodeOf: self class >> #methodWithIfTrueIfFalse.
	self caretAt: 1.

	self assert: self selectedSelector equals: #methodWithIfTrueIfFalse
]
