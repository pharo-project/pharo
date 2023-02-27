"
SUnit tests for the RBDumpVisitor visit, called by the #dump method on RBProgramNodes.
"
Class {
	#name : #RBDumpVisitorTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Visitors'
}

{ #category : #tests }
RBDumpVisitorTest >> compiler [

	^ Smalltalk compiler
]

{ #category : #'test - api' }
RBDumpVisitorTest >> testAPI [

	| node |
	node := self parseExpression: 'self foo'.
	self
		assert: (RBDumpVisitor new visit: node) contents
		equals:  'RBMessageNode receiver: (RBVariableNode named: #self) selector: #foo'
]

{ #category : #tests }
RBDumpVisitorTest >> testArrayNodeDump [

	| node dumpedNode |
	"Empty Array"
	node := self parseExpression: '{}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"non-empty Array"
	node := self parseExpression: '{1 + 1. true. Object new}'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testAssignmentNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a := 3.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBAssignmentNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testBlockNodeDump [

	| node dumpedNode |
	"Simple block"
	node := self parseExpression: '[self]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with argument"
	node := self parseExpression: '[:each | each]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Block with arguments and temps"
	node := self parseExpression: '[:each :i | |a b| a := each. b := i.]'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBBlockNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testCascadeNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self foo; bar'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBCascadeNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testDumpOnObjectMethod [

	| node dumpedNode |
	node := (Object >> #readSlot:) ast.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testDumpOnSelfClassMethods [

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
		self assert: dumpedNode class equals: RBMethodNode.
		self assert: node class equals: dumpedNode class.
		self assert: node equals: dumpedNode ]
]

{ #category : #tests }
RBDumpVisitorTest >> testErrorNodeDump [

	| node dumpedNode |
	node := self parseFaultyExpression: '( +'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBParenthesesErrorNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testGlobalNodeDump [

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

{ #category : #tests }
RBDumpVisitorTest >> testLiteralArrayNodeDump [

	| node dumpedNode |
	node := self parseExpression: '#(1 $a true ''a'')'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralArrayNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testLiteralValueNodeDump [

	| node dumpedNode |
	"Numeric are literals"
	node := self parseExpression: '1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Symbol are literals"
	node := self parseExpression: '#foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Booleans are literals"
	node := self parseExpression: 'true'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"char are literals"
	node := self parseExpression: '$a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"String are literals"
	node := self parseExpression: '''a'''.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBLiteralValueNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testMessageNodeDump [

	| node dumpedNode |
	"Simple selector"
	node := self parseExpression: 'self foo'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With an argument"
	node := self parseExpression: 'self foo: 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"With many arguments"
	node := self parseExpression: 'self foo: 1 bar: 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.

	"Binary message"
	node := self parseExpression: '1 + 2'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMessageNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testMethodNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBMethodNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testPragmaNodeDump [

	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode pragmas first class equals: RBPragmaNode.
	self
		assert: node pragmas first class
		equals: dumpedNode pragmas first class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testReturnNodeDump [

	| node dumpedNode |
	node := self parseExpression: '^ 1 + 1'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBReturnNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testSelfNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'self'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testSequenceNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'foo. bar.'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBSequenceNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testSuperNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'super'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testThisContextNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'thisContext'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> testVariableNodeDump [

	| node dumpedNode |
	node := self parseExpression: 'a'.
	dumpedNode := self compiler evaluate: node dump.

	self assert: dumpedNode class equals: RBVariableNode.
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString
]

{ #category : #tests }
RBDumpVisitorTest >> uselessMethod [
	"used for test"
]
