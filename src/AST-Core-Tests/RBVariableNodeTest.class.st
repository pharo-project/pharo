"
SUnit tests for RBVariableNode
"
Class {
	#name : #RBVariableNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBVariableNodeTest >> testIsDefinition [
	| ast temps |
	ast := self class compiler
		parse:
			'myMethod: arg
  | test |
  test := 2.
  test.
  ^test'.
	ast doSemanticAnalysis.
	temps := ast allChildren select: #isTemp.
	
	"arguments define variables"
	self assert: ast arguments first isDefinition.
	"this is the || definition"
	self assert: temps first isDefinition.
	"all the rest are just uses"
	self deny: temps second isDefinition.
	self deny: temps third isDefinition.
	self deny: temps fourth isDefinition
]

{ #category : #tests }
RBVariableNodeTest >> testStartForReplacement [

	| source tree dTemporary |
	source := 'foo
	|a b c de d|
	de := 4'.
	tree := self parseMethod: source.
	dTemporary := tree body temporaries last.
	self assert: dTemporary name equals: #d.
	self assert: (source copyFrom: dTemporary start to: dTemporary stop) equals: 'd'.
	self assert: (source copyFrom: dTemporary startForReplacement to: dTemporary stop) equals: ' d'.
	self assert: (dTemporary start - dTemporary startForReplacement) equals: 1
]

{ #category : #tests }
RBVariableNodeTest >> testStartForReplacement02 [

	| source tree dTemporary |
	source := 'foo
	|a b c de 
	
d|
	de := 4'.
	tree := self parseMethod: source.
	dTemporary := tree body temporaries last.
	self assert: dTemporary name equals: #d.
	self assert: (source copyFrom: dTemporary start to: dTemporary stop) equals: 'd'.
	self assert: (source copyFrom: dTemporary startForReplacement to: dTemporary stop) equals: ' 
	
d'.
	self assert: (dTemporary start - dTemporary startForReplacement) equals: 4
]
