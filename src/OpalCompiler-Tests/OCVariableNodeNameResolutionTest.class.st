"
SUnit tests for ASTVariableNode
"
Class {
	#name : 'OCVariableNodeNameResolutionTest',
	#superclass : 'OCParseTreeTest',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'tests' }
OCVariableNodeNameResolutionTest >> testIsDefinedByBlock [

	| ast temps args |
	ast := self class compiler parse: 'myMethod: arg
  | test |
  test := 2.
  test.
	[ :test2 | | test3| test2. test3].
  ^test'.
	temps := ast allChildren select: [ :each | each isTempVariable ].
	"test is defined by the method"
	self deny: temps first isDefinedByBlock.
	self deny: temps second isDefinedByBlock.
	self deny: temps third isDefinedByBlock.

	"test3 definition"
	self assert: ast blockNodes first arguments first isDefinedByBlock.
	"test3 definition"
	self assert: temps fourth isDefinedByBlock.
	"test 3 use"
	self assert: temps fifth isDefinedByBlock.
	"and we are out if the block"
	self deny: temps sixth isDefinedByBlock.

	args := ast allChildren select: [ :each | each isArgumentVariable ].
	"method arg"
	self deny: args first isDefinedByBlock.
	self assert: args second isDefinedByBlock.
	"use of test3"
	self assert: args third isDefinedByBlock
]

{ #category : 'tests' }
OCVariableNodeNameResolutionTest >> testIsDefinition [

	| ast temps |
	ast := self class compiler parse: 'myMethod: arg
  | test testCopied testTempVector|
  test := 2.
  test.
  [ testCopied. testTempVector := 1 ].
  ^test'.
	temps := ast allChildren select: [ :each | each isTempVariable ].

	"arguments define variables"
	self assert: ast arguments first isDefinition.
	"this is the || definition"
	self assert: temps first variable class identicalTo: TemporaryVariable.
	self assert: temps first isDefinition.
	self assert: temps second variable class identicalTo: CopiedLocalVariable.
	self assert: temps second isDefinition.
	self assert: temps third variable class identicalTo: OCVectorTempVariable .
	self assert: temps third isDefinition.

	"all the rest are just uses"
	self deny: temps fourth isDefinition.
	self deny: (temps at: 5) isDefinition.
	self deny: (temps at: 6) isDefinition
]

{ #category : 'tests' }
OCVariableNodeNameResolutionTest >> testIsDefinitionAfterNameResolution [

	| ast temps |
	ast := self class compiler parse: 'myMethod: arg
  | test testCopied testTempVector|
  test := 2.
  test.
  [ testCopied. testTempVector := 1 ].
  ^test'.
	temps := ast allChildren select: [ :each | each isTempVariable ].

	"arguments define variables"
	self assert: ast arguments first isDefinition.
	"this is the || definition"
	self assert: temps first variable class identicalTo: TemporaryVariable.
	self assert: temps first isDefinition.
	self assert: temps second variable class identicalTo: CopiedLocalVariable.
	self assert: temps second isDefinition.
	self assert: temps third variable class identicalTo: OCVectorTempVariable .
	self assert: temps third isDefinition.
	"all the rest are just uses"
	self deny: temps fourth isDefinition.
	self deny: (temps at: 5) isDefinition.
	self deny: (temps at: 6) isDefinition
]
