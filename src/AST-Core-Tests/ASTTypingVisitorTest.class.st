"
An ASTTypingVisitorTest is a test class for testing the behavior of ASTTypingVisitor
"
Class {
	#name : #ASTTypingVisitorTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Type'
}

{ #category : #asserting }
ASTTypingVisitorTest >> assertTypedTree: aNode [

	aNode nodesDo: [ :node |
		(self expectedNodeType: node) ifNotNil: [ :type |
			self
				assert: (node propertyAt: #type ifAbsent: [ nil ])
				equals: type ] ]
]

{ #category : #examples }
ASTTypingVisitorTest >> exampleLoop [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"

	| tmp |
	tmp := true "<True>".
	5 timesRepeat: [ :i |
		i = 2 ifTrue: [ ^ tmp "<Boolean>" "need fixed point" ].
		tmp := false "<False>" ]. 
	^ tmp "<Boolean>"
]

{ #category : #examples }
ASTTypingVisitorTest >> exampleMethod [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"

	| collection tmp |
	
	'hello' "<ByteString>".
	self "<ASTTypingVisitorTest>".
	nil "<UndefinedObject>".
	true "<True>".
	
	(tmp := #foo) "<ByteSymbol>" "Assigments are values".
	[ tmp "<ByteSymbol>" ] "<BlockClosure>"
		on: Error "<Error class>"
		do: [ :e | e "<>" "No block analysis" ].

	collection := OrderedCollection "<OrderedCollection class>" new "<OrderedCollection>".
	collection "<OrderedCollection>" add: 1 "<SmallInteger>".
	(collection at: 1) "<>" "no infered type here".
	^ ((self "<ASTTypingVisitorTest>") class "<ASTTypingVisitorTest class>") isNil "<Boolean>"
]

{ #category : #examples }
ASTTypingVisitorTest >> exampleMethod2 [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"

	| tmp |
	tmp "<>" "unknown".
	tmp := { 5 } "<Array>".
	tmp "<Array>".
	tmp := 'five' "<ByteString>".
	tmp "<ArrayedCollection>" "not flow-sensitive, so a common super-class is used".
	
	self exampleMethod "<Boolean>" "Can handle simple inter-procedural calls"
]

{ #category : #utilities }
ASTTypingVisitorTest >> expectedNodeType: aNode [
	"Check that all node with a comment ""<SomeType>"" have the correct corresponding `#type` property."

	(aNode isValue and: [ aNode hasComments ]) ifFalse: [ ^ nil ].
	aNode comments
		select: [ :comment |
			comment contents first = $< and: [ comment contents last = $> ] ]
		thenDo: [ :comment |
			^ self class compiler evaluate:
				  (comment contents copyFrom: 2 to: comment contents size - 1) ].
	^ nil
]

{ #category : #tests }
ASTTypingVisitorTest >> testExampleLoop [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleLoop) parseTree.
	typeVisitor fixedPointAnalysis: ast.
	self assertTypedTree: ast
]

{ #category : #tests }
ASTTypingVisitorTest >> testExampleMethod [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleMethod) parseTree.
	typeVisitor visit: ast.
	self assertTypedTree: ast
]

{ #category : #tests }
ASTTypingVisitorTest >> testExampleMethod2 [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleMethod) parseTree.
	typeVisitor visit: ast.

	ast := (self class >> #exampleMethod2) parseTree.
	typeVisitor visit: ast.
	self assertTypedTree: ast
]
