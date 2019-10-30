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
		controller: CompletionEngine new
		class: NECTestClass
		source: aString
		position: anInteger
]

{ #category : #tests }
CompletionContextTest >> testCreateModel [
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
CompletionContextTest >> testExpressionInIncompleteParanthesis [
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
CompletionContextTest >> testReceiverArgument [
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
CompletionContextTest >> testReceiverCascade [
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
CompletionContextTest >> testReceiverClassVar [
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
CompletionContextTest >> testReceiverConstant [
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
CompletionContextTest >> testReceiverGlobal [
	| text context model |
	text := 'testIt
	Dictionary n'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: model title equals: 'Dictionary class'.

]

{ #category : #tests }
CompletionContextTest >> testReceiverGlobalVarNew [
	| text context model |
	text := 'testIt
	Dictionary new fl'.
	context := self createContextFor: text at: text size.
	model := context model.
	self assert: ((model entries collect: #contents) includes: #flatCollect:).
]

{ #category : #tests }
CompletionContextTest >> testUntypedSelectorsOnly [
	| text context model |
	text := 'testIt: aRectangle
	| abs bcs bds |
	ab'.
	context := self createContextFor: text at: text size.
	model := context model.
	context narrowWith: 'ab'.
	self assert: ((model entries collect: #contents) includes: #abs)
]
