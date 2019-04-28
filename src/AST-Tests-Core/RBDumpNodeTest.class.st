"
SUnit tests for the RBDumpVisitor visit, called by the #dump method on RBProgramNodes.
"
Class {
	#name : #RBDumpNodeTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #tests }
RBDumpNodeTest >> testArrayNodeDump [
	| node dumpedNode |
	"Empty Array"
	node := RBParser parseExpression: '{}'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"non-empty Array"
	node := RBParser parseExpression: '{1 + 1. true. Object new}'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testAssignmentNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'a := 3.'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBAssignmentNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testBlockNodeDump [
	| node dumpedNode |
	"Simple block"
	node := RBParser parseExpression: '[self]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Block with argument"
	node := RBParser parseExpression: '[:each | each]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Block with arguments and temps"
	node := RBParser parseExpression: '[:each :i | |a b| a := each. b := i.]'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBBlockNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
]

{ #category : #tests }
RBDumpNodeTest >> testCascadeNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'self foo; bar'.
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
	node := RBParser parseFaultyExpression: '( +'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBParseErrorNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testGlobalNodeDump [
	| node dumpedNode |
	"Global nodes are only generated when a semantic analysis is triggered on a method"
	node := RBParser parseMethod: 'foo ^ Object'.
	dumpedNode := Smalltalk compiler evaluate: node doSemanticAnalysis dump.
	
	self assert: dumpedNode statements first value class equals: RBGlobalNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testLiteralArrayNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: '#(1 $a true ''a'')'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralArrayNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testLiteralValueNodeDump [
	| node dumpedNode |
	"Numeric are literals"
	node := RBParser parseExpression: '1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Symbol are literals"
	node := RBParser parseExpression: '#foo'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Booleans are literals"
	node := RBParser parseExpression: 'true'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"char are literals"
	node := RBParser parseExpression: '$a'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"String are literals"
	node := RBParser parseExpression: '''a'''.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBLiteralValueNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
]

{ #category : #tests }
RBDumpNodeTest >> testMessageNodeDump [
	| node dumpedNode |
	"Simple selector"
	node := RBParser parseExpression: 'self foo'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"With an argument"
	node := RBParser parseExpression: 'self foo: 1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"With many arguments"
	node := RBParser parseExpression: 'self foo: 1 bar: 2'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	"Binary message"
	node := RBParser parseExpression: '1 + 2'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMessageNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testMethodNodeDump [
	| node dumpedNode |
	node := RBParser parseMethod: 'foo <useless>'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBMethodNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testPragmaNodeDump [
	| node dumpedNode |
	node := RBParser parseMethod: 'foo <useless>'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode pragmas first class equals: RBPragmaNode. 
	self assert: node pragmas first class equals: dumpedNode pragmas first class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testReturnNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: '^ 1 + 1'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBReturnNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testSelfNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'self'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSelfNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testSequenceNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'foo. bar.'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSequenceNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
	
	
]

{ #category : #tests }
RBDumpNodeTest >> testSuperNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'super'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBSuperNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testThisContextNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'thisContext'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBThisContextNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> testVariableNodeDump [
	| node dumpedNode |
	node := RBParser parseExpression: 'a'.
	dumpedNode := Smalltalk compiler evaluate: node dump.
	
	self assert: dumpedNode class equals: RBVariableNode. 
	self assert: node class equals: dumpedNode class.
	self assert: node printString equals: dumpedNode printString.
	
]

{ #category : #tests }
RBDumpNodeTest >> uselessMethod [
	<useless>
	
]
