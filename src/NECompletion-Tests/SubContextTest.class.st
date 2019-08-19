Class {
	#name : #SubContextTest,
	#superclass : #CompletionContextTest,
	#category : #'NECompletion-Tests'
}

{ #category : #testing }
SubContextTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #private }
SubContextTest >> createContextFor: aString at: anInteger [ 
	^ CompletionContext
		controller: CompletionController new
		class: NECTestClass
		source: aString
		position: anInteger
]

{ #category : #tests }
SubContextTest >> testBlockTemporaries [
	| text temps context |
	self skip.
	text := 'testIt
	| a b c |
	a _ [ :each | |d | ^d].
	^self'.
	context := self 
		createContextFor: text
		at: 39.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 5.
	self assert: temps first = 'a'.
	self assert: temps second = 'b'.
	self assert: temps third = 'c'.
	self assert: temps fourth = 'each'.
	self assert: temps fifth = 'd'
]

{ #category : #tests }
SubContextTest >> testBlockTemporariesBoxed [
	| text temps context |
	self skip.
	text := 'testIt
	| a b c |
	a := [ :each | |d | d = a ifTrue:[ |x|  ] ifFalse:[ |y|.
	^self'.
	context := self 
		createContextFor: text
		at: 73.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 5.
	self assert: temps first = 'a'.
	self assert: temps second = 'b'.
	self assert: temps third = 'c'.
	self assert: temps fourth = 'each'.
	self assert: temps fifth = 'd'.

]

{ #category : #tests }
SubContextTest >> testCompletionToken [
	| text context |
	self skip: 'not sure why this fails'.
	text := 'testIt: aRectangle
	aRe'.
	context := self createContextFor: text at: text size.
	self assert: context completionToken equals: 'aRe'
]

{ #category : #tests }
SubContextTest >> testCreateModel [
	| text context model |
	text := 'testIt: aRectangle
	aRectangle printS'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: ''.
	
	text := 'testIt: aRectangle
	1 ad'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: 'SmallInteger'.

	text := 'testIt'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: 'testItems').

	text := 'testIt: rect
	rect is'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: ''
]

{ #category : #tests }
SubContextTest >> testEmpty [
	self skip: 'does this test make sense?'.
	self createContextFor: '' at: 0
]

{ #category : #tests }
SubContextTest >> testExpressionInIncompleteParanthesis [
	| text context model |
	text := 'testIt
	1 + (1 si'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) isEmpty).
]

{ #category : #tests }
SubContextTest >> testInTheMiddelOfAWord [
	| text context |
	self skip: 'we do this at a different point, so idk if this makes sense'.
	text := 'hagada'.
	context := self 
		createContextFor: text
		at: 4.
	self assert: context completionToken equals: 'haga'
]

{ #category : #tests }
SubContextTest >> testReceiverArgument [
	| text context model |
	text := 'testIt: aRectangle
	aRectangle printS'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #printString).
	
	text := 'testIt: rect
	rect is'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: model title equals: ''
]

{ #category : #tests }
SubContextTest >> testReceiverCascade [
	| text context model |
	text := 'testIt
	| b |
	b := Stream new.
	b nextPutA'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: ''.
	
	text := 'testIt
	| b |
	b := Stream new nextPut'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: ''.
	
	text := 'testIt: aStream
	aStream nextPut'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: ''.
]

{ #category : #tests }
SubContextTest >> testReceiverClassVar [
	| text context model |
	text := 'testIt
	ClassAnnotation so'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: model title equals: 'ClassAnnotation class'
]

{ #category : #tests }
SubContextTest >> testReceiverConstant [
	| text context model |
	text := 'testIt
	15r16 as'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #as31BitSmallInt).
	
	text := 'testIt
	''test'' pre'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #prefixMatchesRegex:).
	
	text := 'testIt
	true ifTr'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #ifTrue:ifFalse:).
	
	text := 'testIt
	false  "this is it"
		xo'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #xor:).
	
	text := 'testIt
	a := [ :test | test * test ] 
		as'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #asMinimalRepresentation).
	
	text := 'testIt
	$c as'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #asciiValue).
	
	text := 'testIt
	#gaga un'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #uncapitalized).
	
	text := 'testIt
	#( 1 2 3) as'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #asByteArray).
	
	text:='testIt
	2r1.1e2 ba'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #basicIdentityHash).
	
	text:='testIt
	23s32 as'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #asFraction).
]

{ #category : #tests }
SubContextTest >> testReceiverGlobal [
	| text context model |
	text := 'testIt
	Dictionary n'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: 'Dictionary class'.

]

{ #category : #tests }
SubContextTest >> testReceiverGlobalVarNew [
	| text context model |
	text := 'testIt
	Dictionary new fl'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #flatCollect:).
]

{ #category : #tests }
SubContextTest >> testReceiverTempVar [
	| text context |
	self skip.
	text := 'testIt
	| aha |
	aha _ ''test''.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha |
	ah _ ''test''.
	ah p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	text := 'testIt
	| aha |
	aha _ constantString.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha |
	aha _ constant asDate.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ aha.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ aha _ 15.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass = SmallInteger.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ 15.
	bili _ aha.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == SmallInteger.
	text := 'testIt
	[ :each | |a| a _ 16. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == SmallInteger.
	text := 'testIt
	[ :each | |a| a _ Dictionary new. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == Dictionary.
	text := 'testIt
	[ :each | |a| a _ Dictionary. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == Dictionary class.
	text := 'testIt
	| aha |
	aha _ constantLargeInteger.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass = LargePositiveInteger.
]

{ #category : #tests }
SubContextTest >> testReceiverTempVar2 [
	| text context model |
	self skip: 'check why fails'.
	text := 'openMenuFor: aParagraphEditor 
	| theMenu |
	context := NECContext
				controller: self
				class: model receiverClass
				source: aParagraphEditor text string
				position: aParagraphEditor caret - 1.
	editor := aParagraphEditor.
	theMenu := NECMenuMorph
				controller: self
				position: (aParagraphEditor selectionPosition: context completionToken).
	theMenu isClosed
		ifFalse: [menuMorph := theMenu]. theMenu ins'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #insertCommonPrefixOrSelected)
]

{ #category : #tests }
SubContextTest >> testReceiverTempVar3 [
"if tempvar assignment is from a keyword message send (case 3), the guessed type should be 'nil' as in case for simple messages. Don't move over to other var assignments (smallint for var b)"
	| text context model |
	self skip: 'check why fails'.
	"case 1 guess from constant"
	text := 'testIt
	| a b |
	a := ''test''.
	b := 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	"case 2 can not guess from return value of a message send"
	text := 'testIt
	| a b |
	a _ self class.
	b _ 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	"case 3 can not guess from return value of a keyword message send"
	text := 'testIt
	| a b |
	a := self init:''test''.
	b := 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: model title equals: ''
]

{ #category : #tests }
SubContextTest >> testTemporaries [
	| text temps context |
	self skip.
	text := 'testIt: aRectangle
	| a b c |
	a _ [ :each | |d | ^d].
	^self'.
	context := self 
		createContextFor: text
		at: text size.
	temps := context temporaries collect: [ :each | each contents ].
	self assert: temps size = 4.
	self assert: temps first = 'aRectangle'.
	self assert: temps second = 'a'.
	self assert: temps third = 'b'.
	self assert: temps fourth = 'c'
]

{ #category : #tests }
SubContextTest >> testUnfinishedString [
	| text context model |
	self skip: 'does this even make sense?'.
	text := 'testIt: aRectangle
	| a b c |
	self test: ''unfi'.
	context := self 
		createContextFor: text
		at: text size.
	model := context model.
	self assert: (model entries collect: #contents) isEmpty.
]

{ #category : #tests }
SubContextTest >> testUntypedSelectorsOnly [
	| text context model |
	text := 'testIt: aRectangle
	| abs bcs bds |
	ab'.
	context := self createContextFor: text at: text size.
	model := context model.
	context narrowWith: 'ab'.
	self assert: ((model entries collect: #contents) includes: #abs)
]

{ #category : #tests }
SubContextTest >> testUntypedVarsOnly [
	self skip: 'does this test make sense?'.
	self checkUntypedVariablesOnly: 'testIt
	'.
	self checkUntypedVariablesOnly: '+ aTest
	'.
	self checkUntypedVariablesOnly: 'gaga: aTest
	'.
	self checkUntypedVariablesOnly: 'gaga
	^ '.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ '.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each'.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each |'.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each | '.
	self checkUntypedVariablesOnly: 'testIt
	<primitive: 67> 
	'.
	self checkUntypedVariablesOnly: 'testIt
	ab _'.
	self checkUntypedVariablesOnly: 'testIt
	ab _ '.
	self checkUntypedVariablesOnly: 'self compare: '.
	self checkUntypedVariablesOnly: 'self compare: x caseSensitive: '.
	self checkUntypedVariablesOnly: 'self gaga: x gugu: ('.
	self checkUntypedVariablesOnly: 'testIt
	a _ 15 + '.
	self checkUntypedVariablesOnly: 'testIt
	self hugatada: '
]
