"
SUnit tests for the ASTDumpVisitor visit, called by the #dump method on ASTProgramNodes.
"
Class {
	#name : 'OCDumpVisitorTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
OCDumpVisitorTest >> compiler [

	^ Smalltalk compiler
]

{ #category : 'tests - api' }
OCDumpVisitorTest >> testAPI [

	| node |
	node := self parseExpression: 'self foo'.
	self
		assert: (OCDumpVisitor new visit: node) contents
		equals: 'OCMessageNode receiver: (OCVariableNode named: #self) selector: #foo'
]

{ #category : 'tests' }
OCDumpVisitorTest >> testArrayNodeDump [

	| node dumpedNode |
	"Empty Array"
	node := self parseExpression: '{}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"non-empty Array"
	node := self parseExpression: '{1 + 1. true. Object new}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testAssignmentNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a := 3.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCAssignmentNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testBlockNodeDump [

	| node dumpedNode |
	"Simple block"
	node := self parseExpression: '[self]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with argument"
	node := self parseExpression: '[:each | each]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with arguments and temps"
	node := self parseExpression: '[:each :i | |a b| a := each. b := i.]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testCascadeNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self foo; bar'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCCascadeNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testDumpOnObjectMethod [

	| node dumpedNode |
	node := (Object >> #readSlot:) ast.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testDumpOnSelfClassMethods [

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
		self assert: dumpedNode class equals: OCMethodNode.
		self assert: node class equals: dumpedNode class.
		self assert: node equals: dumpedNode ]
]

{ #category : 'tests' }
OCDumpVisitorTest >> testErrorNodeDump [

	| node dumpedNode |
	node := self parseFaultyExpression: '( +'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCParenthesesErrorNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testGlobalNodeDump [

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
OCDumpVisitorTest >> testLiteralArrayNodeDump [

	| node dumpedNode |
	node := self parseExpression: '#(1 $a true ''a'')'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testLiteralValueNodeDump [

	| node dumpedNode |
	"Numeric are literals"
	node := self parseExpression: '1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Symbol are literals"
	node := self parseExpression: '#foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Booleans are literals"
	node := self parseExpression: 'true'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"char are literals"
	node := self parseExpression: '$a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"String are literals"
	node := self parseExpression: '''a'''.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testMessageNodeDump [

	| node dumpedNode |
	"Simple selector"
	node := self parseExpression: 'self foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With an argument"
	node := self parseExpression: 'self foo: 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With many arguments"
	node := self parseExpression: 'self foo: 1 bar: 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Binary message"
	node := self parseExpression: '1 + 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testMethodNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testPragmaNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode pragmas first class equals: OCPragmaNode.
	self
		assert: node pragmas first class
		equals: dumpedNode pragmas first class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testReturnNodeDump [

	| node dumpedNode |
	node := self parseExpression: '^ 1 + 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCReturnNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testSelfNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testSequenceNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'foo. bar.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCSequenceNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testSuperNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'super'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testThisContextNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'thisContext'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> testVariableNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: OCVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : 'tests' }
OCDumpVisitorTest >> uselessMethod [
	"used for test"
]
