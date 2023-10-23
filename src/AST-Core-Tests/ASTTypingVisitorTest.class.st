"
An ASTTypingVisitorTest is a test class for testing the behavior of ASTTypingVisitor
"
Class {
	#name : 'ASTTypingVisitorTest',
	#superclass : 'TestCase',
	#category : 'AST-Core-Tests-Type',
	#package : 'AST-Core-Tests',
	#tag : 'Type'
}

{ #category : 'asserting' }
ASTTypingVisitorTest >> assertTypedTree: aNode [

	aNode nodesDo: [ :node |
		(self expectedNodeType: node) ifNotNil: [ :type |
			self
				assert: (node propertyAt: #type ifAbsent: [ nil ])
				equals: type ] ]
]

{ #category : 'examples' }
ASTTypingVisitorTest >> exampleCHA [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"

	| bool |
	true "<True>".
	true not "<False>".

	bool := self hash > 1000.
	bool "<Boolean>".
	bool not "<Boolean>" "Without CHA, type is nil as Boolean>>#not is subclass responsibility"
]

{ #category : 'examples' }
ASTTypingVisitorTest >> exampleKernel: arg1 and: arg2 [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"
	"List of special kernel method"
	
	(arg1 = arg2) "<Boolean>".
	(arg1 ~= arg2) "<Boolean>".
	(arg1 == arg2) "<Boolean>".
	(arg1 ~~ arg2) "<Boolean>".
	(arg1 < arg2) "<Boolean>".
	(arg1 <= arg2) "<Boolean>".
	(arg1 > arg2) "<Boolean>".
	(arg1 >= arg2) "<Boolean>".
	(arg1 isNil) "<Boolean>".
	(arg1 isNotNil) "<Boolean>".
	(arg1 isEmpty) "<Boolean>".
	(arg1 size) "<Integer>".
	(arg1 at: arg2) "<>". "not special"

	(5 yourself) "<SmallInteger>".
	(5 class) "<SmallInteger class>".
	(Object new) "<Object>".
	(Array new: 1) "<Array>".
	(Object basicNew) "<Object>".
	(Array basicNew: 1) "<Array>".
	(5 at: 1) "<>" "not special".
	(Object at: 1) "<>" "not special"
]

{ #category : 'examples' }
ASTTypingVisitorTest >> exampleLoop [
	"DO NOT REFORMAT, or type expectations (in special comments) will be lost"

	| tmp |
	tmp := true "<True>".
	5 timesRepeat: [ :i |
		i = 2 ifTrue: [ ^ tmp "<Boolean>" "need fixed point" ].
		tmp := false "<False>" ]. 
	^ tmp "<Boolean>"
]

{ #category : 'examples' }
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

{ #category : 'examples' }
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

{ #category : 'utilities' }
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

{ #category : 'tests' }
ASTTypingVisitorTest >> testExampleCHA [

	| typeVisitor ast methods |
	typeVisitor := ASTTypingVisitor new.
	typeVisitor cha: true.
	methods := {
		           (Boolean >> #not).
		           (True >> #not).
		           (False >> #not).
		           (self class >> #exampleCHA) }.
	methods do: [ :m |
		ast := m parseTree.
		typeVisitor fixedPointAnalysis: ast ].
	self assertTypedTree: ast
]

{ #category : 'tests' }
ASTTypingVisitorTest >> testExampleKernel [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleKernel:and:) parseTree.
	typeVisitor visitNode: ast.
	self assertTypedTree: ast
]

{ #category : 'tests' }
ASTTypingVisitorTest >> testExampleLoop [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleLoop) parseTree.
	typeVisitor fixedPointAnalysis: ast.
	self assertTypedTree: ast
]

{ #category : 'tests' }
ASTTypingVisitorTest >> testExampleMethod [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleMethod) parseTree.
	typeVisitor visit: ast.
	self assertTypedTree: ast
]

{ #category : 'tests' }
ASTTypingVisitorTest >> testExampleMethod2 [

	| typeVisitor ast |
	typeVisitor := ASTTypingVisitor new.
	ast := (self class >> #exampleMethod) parseTree.
	typeVisitor visit: ast.

	ast := (self class >> #exampleMethod2) parseTree.
	typeVisitor visit: ast.
	self assertTypedTree: ast
]
