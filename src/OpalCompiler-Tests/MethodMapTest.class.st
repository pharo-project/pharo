Class {
	#name : 'MethodMapTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'utilities' }
MethodMapTest >> compileAndRunExample: aSelector [
	| oldMethod recompiledMethod |
	oldMethod := MethodMapExamples>>aSelector.
	recompiledMethod := oldMethod methodClass compiler compile: oldMethod sourceCode.
	^recompiledMethod valueWithReceiver: MethodMapExamples new
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> deadContext [
	^ thisContext
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> inlinedBlockSourceNode [
	1 to: 1 do: [ :i | ^ thisContext sourceNode ]
]

{ #category : 'helpers' }
MethodMapTest >> parseExpression: aString [

	^ OCParser parseExpression: aString
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testBlockAndContextSourceNode [
	| block blockNodeViaContext blockNodeViaClosure |
	block := [ blockNodeViaContext := thisContext sourceNode ].
	block value.
	blockNodeViaClosure := block sourceNode.

	self assert: blockNodeViaContext identicalTo: blockNodeViaClosure
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testBlockSourceNode [
	| sourceNode |
	sourceNode := [ 1 + 2 ] sourceNode.
	self assert: sourceNode equals: (self parseExpression: '[ 1 + 2 ]')
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testBlockWithArgAndEnclosedBlockSourceNode [
	| sourceNode |
	sourceNode := [ :arg |  [ arg ] ] sourceNode.
	self assert: sourceNode equals: (self parseExpression: '[ :arg | [ arg ] ]')
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testBlockWithEnclosedBlockSourceNode [
	| sourceNode |
	sourceNode := [ [ ] ] sourceNode.
	self assert: sourceNode equals: (self parseExpression: '[ [ ] ]')
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testBlockWithTempsSourceNode [
	| sourceNode |
	sourceNode := [ | t1 t2 | ] sourceNode.
	self assert: sourceNode equals: (self parseExpression: '[ | t1 t2 | ]')
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testCompiledBlockSourceNode [

	| method block |
	"if we compile a method, but to not install it, make sure that we can map the block to the AST"

	method := OpalCompiler new compile: 'foo Object. [ self ]. ^ #bar'.
	block := method literals detect: #isCompiledBlock.
	self assert: block sourceNode equals: (self parseExpression: '[ self ]')
]

{ #category : 'tests - temp access' }
MethodMapTest >> testCopiedVarFromDeadContext [
	self assert:  (self compileAndRunExample:  #exampleCopiedVarFromDeadContext) equals: 2
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testDeadContextSourceNode [
	| deadContext |
	deadContext := self deadContext.
	self assert: deadContext isDead.
	self assert: deadContext sourceNode equals: (self class>>#deadContext) ast
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleAccessOuterFromCleanBlock [
	self assert: (self compileAndRunExample: #exampleAccessOuterFromCleanBlock) equals: 1
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleSimpleTemp [
	self assert: (self compileAndRunExample: #exampleSimpleTemp) equals: 1
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedCopying [
	self assert: (self compileAndRunExample: #exampleTempNamedCopying) equals: 1
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedCopying2 [
	self assert: (self compileAndRunExample: #exampleTempNamedCopying2) equals: 1
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedPutCopying [
	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedPutCopying2 [
	"modifying a copied temp variable will modify the value in the outer context"

	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying2) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedPutCopying3 [
	"modifying a copied temp variable will modify the value in the outer context"

	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying3) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedPutTempVector [
	self assert: (self compileAndRunExample: #exampleTempNamedPutTempVector) equals: 3
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedPutTempVector2 [
	self assert: (self compileAndRunExample: #exampleTempNamedPutTempVector2) equals: 3
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedTempVector [
	self assert: (self compileAndRunExample: #exampleTempNamedTempVector) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedTempVector2 [
	self assert: (self compileAndRunExample: #exampleTempNamedTempVector2) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedTempVectorInOptimizedBlock [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorInOptimizedBlock ) equals: 2
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedTempVectorInlinedLoop [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorInlinedLoop ) equals: 42
]

{ #category : 'tests - temp access' }
MethodMapTest >> testExampleTempNamedTempVectorNestedBlock [
	self assert:  (self compileAndRunExample:  #exampleTempNamedTempVectorNestedBlock ) equals: 2
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testMethodSourceNodeAtInitialPC [

	| method actual |
	method := self class >> testSelector.
	actual := method sourceNodeForPC: method initialPC.

	self assert: actual equals: method ast sendNodes first receiver receiver
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testMethodSourceNodeAtPC [
	self assert: (((Object>>#halt) sourceNodeForPC:  (Smalltalk vm for32bit: 22 for64bit: 42)) isKindOf: OCMessageNode)
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testPrimitiveMethodSourceNodeAtInitialPC [

	| method actual |
	method := SmallInteger >> #+.
	actual := method sourceNodeForPC: method initialPC.

	self assert: actual equals: method ast
]

{ #category : 'tests - temp access' }
MethodMapTest >> testTempNamedTempCopyingNestedBlock [
	self
		assert: (self compileAndRunExample: #exampleTempNamedTempCopyingNestedBlock)
		equals: 1
]

{ #category : 'tests - temp access' }
MethodMapTest >> testTempNamedTempCopyingNestedBlock2 [
	self
		assert: (self compileAndRunExample: #exampleTempNamedTempCopyingNestedBlock2)
		equals: 1
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testThisContextSourceNode [
	self assert: (thisContext sourceNode isKindOf: OCMethodNode).
	self assert: ([thisContext sourceNode] value isKindOf: OCBlockNode).
	self assert: ([true ifTrue: [thisContext sourceNode]]value isKindOf: OCBlockNode)
]

{ #category : 'tests - ast mapping' }
MethodMapTest >> testThisContextSourceNodeInInlinedMessage [
	| inlinedBlockSourceNode |
	"we get the method node as this is what created the context"
	inlinedBlockSourceNode := self inlinedBlockSourceNode.
	self assert: (inlinedBlockSourceNode isKindOf: OCMethodNode)
]
