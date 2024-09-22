Class {
	#name : 'ASTReturnNodeAdderVisitorTest',
	#superclass : 'ASTParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenArrayExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: '{ x . ''a'' . #a }'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ { x . ''a'' . #a }'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenAssignmentExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: 'a := self foo'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ a := self foo'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenBlockExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: '[ a := self foo ]'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ [ a := self foo ]'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenCascadeMessageExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: 'self foo; bar; end'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ self foo; bar; end'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenLiteralExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: '1'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ 1'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenMessageSendExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: 'self foo'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ self foo'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenReturnNodeExpectNoChanges [

	| expression visitor result |
	expression := RBParser parseExpression: '^ 2'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	self assert: expression equals: result
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenSequenceExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: 'self foo. self bar'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: 'self foo. ^ self bar'.
	self assert: result equals: expect
]

{ #category : 'tests' }
ASTReturnNodeAdderVisitorTest >> testAddReturnWhenVariableExpectReturnAdded [

	| expression visitor result expect |
	expression := RBParser parseExpression: 'x'.
	visitor := RBReturnNodeAdderVisitor new.
	result := visitor visit: expression.
	
	expect := RBParser parseExpression: '^ x'.
	self assert: result equals: expect
]
