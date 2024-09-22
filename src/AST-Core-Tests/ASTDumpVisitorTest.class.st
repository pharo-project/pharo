"
SUnit tests for the RBDumpVisitor visit, called by the #dump method on ASTProgramNodes.
"
Class {
	#name : 'ASTDumpVisitorTest',
	#superclass : 'ASTParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
ASTDumpVisitorTest >> compiler [

	^ Smalltalk compiler
]

{ #category : 'tests - api' }
ASTDumpVisitorTest >> testAPI [

	| node |
	node := self parseExpression: 'self foo'.
	self
		assert: (ASTDumpVisitor new visit: node) contents
		equals: 'ASTMessageNode receiver: (ASTVariableNode named: #self) selector: #foo'
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testArrayNodeDump [

	| node dumpedNode |
	"Empty Array"
	node := self parseExpression: '{}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"non-empty Array"
	node := self parseExpression: '{1 + 1. true. Object new}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testAssignmentNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a := 3.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTAssignmentNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testBlockNodeDump [

	| node dumpedNode |
	"Simple block"
	node := self parseExpression: '[self]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with argument"
	node := self parseExpression: '[:each | each]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with arguments and temps"
	node := self parseExpression: '[:each :i | |a b| a := each. b := i.]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testCascadeNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self foo; bar'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTCascadeNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testDumpOnObjectMethod [

	| node dumpedNode |
	node := (Object >> #readSlot:) ast.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testDumpOnSelfClassMethods [

	| methods node dumpedNode |
	methods := {
		           (self class >> #testAssignmentNodeDump).
		           (self class >> #uselessMethod).
		           (self class >> #testVariableNodeDump).
		           (self class >> #testThisContextNodeDump).
		           (self class >> #testReturnNodeDump) }.
	methods do: [ :each |
		node := each ast.
		dumpedNode := self compiler evaluate: node dump.
		self assert: dumpedNode class equals: ASTMethodNode.
		self assert: node class equals: dumpedNode class.
		self assert: node equals: dumpedNode ]
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testErrorNodeDump [

	| node dumpedNode |
	node := self parseFaultyExpression: '( +'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTParenthesesErrorNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testGlobalNodeDump [

	| node dumpedNode |
	"Global nodes are only generated when a semantic analysis is triggered on a method"
	node := self parseMethod: 'foo ^ Object'.
	node doSemanticAnalysis.
	dumpedNode := self compiler evaluate: node dump.
	dumpedNode doSemanticAnalysis.

	self assert: dumpedNode statements first value isGlobalVariable.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testLiteralArrayNodeDump [

	| node dumpedNode |
	node := self parseExpression: '#(1 $a true ''a'')'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testLiteralValueNodeDump [

	| node dumpedNode |
	"Numeric are literals"
	node := self parseExpression: '1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Symbol are literals"
	node := self parseExpression: '#foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Booleans are literals"
	node := self parseExpression: 'true'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"char are literals"
	node := self parseExpression: '$a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"String are literals"
	node := self parseExpression: '''a'''.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testMessageNodeDump [

	| node dumpedNode |
	"Simple selector"
	node := self parseExpression: 'self foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With an argument"
	node := self parseExpression: 'self foo: 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With many arguments"
	node := self parseExpression: 'self foo: 1 bar: 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Binary message"
	node := self parseExpression: '1 + 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testMethodNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testPragmaNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode pragmas first class equals: ASTPragmaNode.
	self
		assert: node pragmas first class
		equals: dumpedNode pragmas first class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testReturnNodeDump [

	| node dumpedNode |
	node := self parseExpression: '^ 1 + 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTReturnNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testSelfNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testSequenceNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'foo. bar.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTSequenceNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testSuperNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'super'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testThisContextNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'thisContext'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> testVariableNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: ASTVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
ASTDumpVisitorTest >> uselessMethod [
	"used for test"
]
