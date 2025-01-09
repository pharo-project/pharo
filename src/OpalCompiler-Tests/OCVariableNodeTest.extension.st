Extension { #name : 'OCVariableNodeTest' }

{ #category : '*OpalCompiler-Tests' }
OCVariableNodeTest >> testIsDefinition [

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

{ #category : '*OpalCompiler-Tests' }
OCVariableNodeTest >> testIsDefinitionAfterNameResolution [

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
