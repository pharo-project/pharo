"
SUnit tests for the RBDumpVisitor visit, called by the #dump method on RBProgramNodes.
"
Class {
	#name : #RBDumpNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBDumpNodeTest >> testArrayNodeDump [
	| node dumpedNode |
	"Empty Array"
	node := self parseExpression: '{}'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"non-empty Array"
	node := self parseExpression: '{1 + 1. true. Object new}'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testAssignmentNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'a := 3.'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBAssignmentNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testBlockNodeDump [
	| node dumpedNode |
	"Simple block"
	node := self parseExpression: '[self]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Block with argument"
	node := self parseExpression: '[:each | each]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Block with arguments and temps"
	node := self parseExpression: '[:each :i | |a b| a := each. b := i.]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
]

{ #category : #tests }
RBDumpNodeTest >> testCascadeNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'self foo; bar'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBCascadeNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testDumpOnObjectMethod [
	| node dumpedNode |
	node := (Object>>#readSlot:) ast.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMethodNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testDumpOnSelfClassMethods [
	| methods node dumpedNode |
	methods := {(self class >> #testAssignmentNodeDump).
	(self class >> #uselessMethod).
	(self class >> #testVariableNodeDump).
	(self class >> #testThisContextNodeDump).
	(self class >> #testReturnNodeDump)}.
	methods
		do: [ :each | 
			node := each ast.
			dumpedNode := Smalltalk compiler evaluate: node dump.
			self assert: dumpedNode class equals: RBMethodNode.
			self assert: node class equals: dumpedNode class.
			self assert: node formattedCode equals: dumpedNode printString ]
]

{ #category : #tests }
RBDumpNodeTest >> testErrorNodeDump [
	| node dumpedNode |
	node := self parseFaultyExpression: '( +'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBParseErrorNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testGlobalNodeDump [
	| node dumpedNode |
	"Global nodes are only generated when a semantic analysis is triggered on a method"
	node := self parseMethod: 'foo ^ Object'.
	dumpedNode := Smalltalk compiler evaluate: node doSemanticAnalysis dump.
	
	self assert: dumpedNode statements first value class equals: RBGlobalNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testLiteralArrayNodeDump [
	| node dumpedNode |
	node := self parseExpression: '#(1 $a true ''a'')'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testLiteralValueNodeDump [
	| node dumpedNode |
	"Numeric are literals"
	node := self parseExpression: '1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Symbol are literals"
	node := self parseExpression: '#foo'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Booleans are literals"
	node := self parseExpression: 'true'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"char are literals"
	node := self parseExpression: '$a'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"String are literals"
	node := self parseExpression: '''a'''.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
]

{ #category : #tests }
RBDumpNodeTest >> testMessageNodeDump [
	| node dumpedNode |
	"Simple selector"
	node := self parseExpression: 'self foo'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"With an argument"
	node := self parseExpression: 'self foo: 1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"With many arguments"
	node := self parseExpression: 'self foo: 1 bar: 2'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Binary message"
	node := self parseExpression: '1 + 2'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testMethodNodeDump [
	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMethodNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testPragmaNodeDump [
	| node dumpedNode |
	node := self parseMethod: 'foo <useless>'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode pragmas first class equals: RBPragmaNode. 
	self assert: node pragmas first class equals: dumpedNode pragmas first class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testReturnNodeDump [
	| node dumpedNode |
	node := self parseExpression: '^ 1 + 1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBReturnNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testSelfNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'self'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSelfNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testSequenceNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'foo. bar.'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSequenceNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testSuperNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'super'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSuperNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testThisContextNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'thisContext'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBThisContextNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testVariableNodeDump [
	| node dumpedNode |
	node := self parseExpression: 'a'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBVariableNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> uselessMethod [
	<useless>
	
]
