Class {
	#name : #CompletionContextTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #testing }
CompletionContextTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #private }
CompletionContextTest >> createContextFor: aString at: anInteger [ 
	^ CompletionContext
		engine: CompletionEngine new
		class: NECTestClass
		source: aString
		position: anInteger
]

{ #category : #tests }
CompletionContextTest >> testExpressionInIncompleteParanthesis [
	| text context |
	text := 'testIt
	1 + (1 si'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: ((context entries collect: #contents) includes: #sin).
]

{ #category : #tests }
CompletionContextTest >> testReceiverArgument [
	| text context |
	text := 'testIt: aRectangle
	aRectangle printS'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: ((context entries collect: #contents) includes: #printString).
	self assert: context title equals: ''.
	
	text := 'testIt: rect
	rect is'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context title equals: ''
]

{ #category : #tests }
CompletionContextTest >> testReceiverClassVar [
	| text context |
	text := 'testIt
	ClassAnnotation so'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context title equals: 'ClassAnnotation class'
]

{ #category : #tests }
CompletionContextTest >> testReceiverConstant [
	| text context |
	text := 'testIt
	15r16 as'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #as31BitSmallInt).
	
	text := 'testIt
	''test'' pre'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #prefixMatchesRegex:).
	
	text := 'testIt
	true ifTr'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #ifTrue:ifFalse:).
	
	text := 'testIt
	false  "this is it"
		xo'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #xor:).
	
	text := 'testIt
	a := [ :test | test * test ] 
		as'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #asMinimalRepresentation).
	
	text := 'testIt
	$c as'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #asciiValue).
	
	text := 'testIt
	#gaga un'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #uncapitalized).
	
	text := 'testIt
	#( 1 2 3) as'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #asByteArray).
	
	text:='testIt
	2r1.1e2 ba'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #basicIdentityHash).
	
	text:='testIt
	23s32 as'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #asFraction).
]

{ #category : #tests }
CompletionContextTest >> testReceiverGlobal [
	| text context |
	text := 'testIt
	Dictionary n'.
	context := self createContextFor: text at: text size.
	self assert: context title equals: 'Dictionary class'.

]

{ #category : #tests }
CompletionContextTest >> testReceiverGlobalVarNew [
	| text context |
	text := 'testIt
	Dictionary new fl'.
	context := self createContextFor: text at: text size.
	self assert: ((context entries collect: #contents) includes: #flatCollect:).
]

{ #category : #tests }
CompletionContextTest >> testReceiverTemp [
	| text context |
	text := 'testIt
	| b |
	b := Stream new.
	b nextPutA'.
	context := self createContextFor: text at: text size.
	self assert: context title equals: ''.
	
	text := 'testIt
	| b |
	b := Stream.
	b nextPutA'.
	context := self createContextFor: text at: text size.
	self assert: context title equals: 'Stream class'.
	
	text := 'testIt: aStream
	aStream nextPut'.
	context := self createContextFor: text at: text size.
	self assert: context title equals: ''.
]

{ #category : #tests }
CompletionContextTest >> testTempVariable [
	| text context |
	text := 'testIt: aRectangle
	| abs bcs bds |
	ab'.
	context := self createContextFor: text at: text size.
	context narrowWith: 'ab'.
	self assert: ((context entries collect: #contents) includes: #abs)
]
