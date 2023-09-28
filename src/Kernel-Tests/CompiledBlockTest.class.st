Class {
	#name : 'CompiledBlockTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
CompiledBlockTest >> testLiteralEqual [
	"Check that if we have two compiledblocks that are equal, there are two in the literals
	and that they are not #literalEqual:"
	| methodToTest compiledBlocks |
	methodToTest := self class compiler compile: 'testWrongHeightlightSingleValue

	| value |
	value := 0.
	"Do not use #to:do: because it is optimized and will prevent us of testing the behavior we want."
	(1 to: 10) do: [self assert: value equals: 0].
	value := 1.
	(1 to: 10) do: [self assert: value equals: 0].'.

	compiledBlocks := methodToTest literals select: [ :each | each isCompiledBlock ].
	self assert: compiledBlocks size equals: 2.
	self deny: (compiledBlocks first literalEqual: compiledBlocks second).
	self assert: compiledBlocks first equals: compiledBlocks second
]

{ #category : 'tests' }
CompiledBlockTest >> testPcInOuter [
	| methodToTest compiledBlocks |
	methodToTest := self class compiler compile: 'testWrongHeightlightSingleValue

	| value |
	value := 0.
	"Do not use #to:do: because it is optimized and will prevent us of testing the behavior we want."
	(1 to: 10) do: [self assert: value equals: 0].
	value := 1.
	(1 to: 10) do: [self assert: value equals: 0].'.

	compiledBlocks := methodToTest literals select: [ :each | each isCompiledBlock ].
	self assert: compiledBlocks size equals: 2.
	"we should find different pcs for the blocks in the outer method"
	self deny: compiledBlocks first pcInOuter equals: compiledBlocks second pcInOuter
]
