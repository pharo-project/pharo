Class {
	#name : 'OCReturnNodeAdderVisitorTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenArrayExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: '{ x . ''a'' . #a }'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ { x . ''a'' . #a }'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenAssignmentExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: 'a := self foo'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ a := self foo'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenBlockExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: '[ a := self foo ]'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ [ a := self foo ]'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenCascadeMessageExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: 'self foo; bar; end'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ self foo; bar; end'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenLiteralExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: '1'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ 1'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenMessageSendExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: 'self foo'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ self foo'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenReturnNodeExpectNoChanges [

	| expression visitor result |
	expression := OCParser parseExpression: '^ 2'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	self assert: expression equals: result
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenSequenceExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: 'self foo. self bar'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: 'self foo. ^ self bar'.
	self assert: result equals: expect
]

{ #category : 'tests' }
OCReturnNodeAdderVisitorTest >> testAddReturnWhenVariableExpectReturnAdded [

	| expression visitor result expect |
	expression := OCParser parseExpression: 'x'.
	visitor := OCReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := OCParser parseExpression: '^ x'.
	self assert: result equals: expect
]
