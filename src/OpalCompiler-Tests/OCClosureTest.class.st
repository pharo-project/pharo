Class {
	#name : 'OCClosureTest',
	#superclass : 'TestCase',
	#instVars : [
		'collection'
	],
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'utilities' }
OCClosureTest >> assertValues: anArray [
	| values |
	values := collection collect: [ :each | each value ].
	self
		assert: anArray asArray = values asArray
		description: ['Expected: ' , anArray asArray printString ,
			', but got ' , values asArray printString]
]

{ #category : 'tests' }
OCClosureTest >> methodArgument: anObject [
	^ [ anObject ]
]

{ #category : 'running' }
OCClosureTest >> setUp [
	super setUp.
	collection := OrderedCollection new
]

{ #category : 'test - clean' }
OCClosureTest >> testActiveHomeClean [
	<compilerOptions: #(+ optionCleanBlockClosure)>
	| block |
	block := [thisContext activeHome method].
	self assert: block class equals: CleanBlockClosure.

	self assert: block value equals: block homeMethod.

	block := [thisContext activeHome].
	self assert: block class equals: CleanBlockClosure.
	self assert: block value equals: thisContext
]

{ #category : 'tests' }
OCClosureTest >> testBlockArgument [
	| block block1 block2 |
	block := [ :arg |
	| temp |
	temp := arg.
	[ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : 'tests' }
OCClosureTest >> testBlockTemp [
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : 'tests' }
OCClosureTest >> testBlockTemps [
	| block block1 block2 |
	"Regression test: Bytecode offset of IR was to last byte of IR node, which for blocks include temp initialization bytes. This caused scan for block creation bytecode to fail when there were many block temps, and no source node to be found."
	block := [ :arg |
	| a b c d e f g |
	a := b := c := d := e := f := g := arg.
	[ a ] ].
	self assert: block sourceNode isBlock.
	self assert: block argumentNames equals: #(#arg).
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : 'test - clean' }
OCClosureTest >> testCleanBlockClosure [
	<compilerOptions: #(+ optionCleanBlockClosure)>
	| block |
	"this is a test that enales optionCleanBlockClosure and check that clean blocks work"
	block := [ 1+2 ].
	self assert: block isClean.
	self assert: block class equals: CleanBlockClosure.
	self assert: block outerContext isNil.
	self assert: block value equals: 3
]

{ #category : 'test - clean' }
OCClosureTest >> testConstantBlockClosure [
	<compilerOptions: #(+ optionCleanBlockClosure)>
	| block |
	"this is a test that enales optionCleanBlockClosure and check that clean blocks work"
	block := [1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block class equals: ConstantBlockClosure.
	self assert: block compiledBlock numTemps equals: 0.
	self assert: block compiledBlock numArgs equals: 0.
	self assert: block outerContext isNil.
	self assert: block value equals: 1.
	self assert: (block valueWithArguments: #()) equals: 1.


	self should: [block value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block valueWithArguments: #(1)] raise: ArgumentsCountMismatch.


	block := [:arg | 1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block class equals: ConstantBlockClosure1Arg.
	self assert: block compiledBlock numTemps equals: 1.
	self assert: block compiledBlock numArgs equals: 1.
	self assert: block outerContext isNil.
	self assert: (block value: nil) equals: 1.
	self assert: (block valueWithArguments: #(2)) equals: 1.

	self should: [block value: nil value: nil ] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch.

	block := [:arg1 :arg2 | 1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block class equals: ConstantBlockClosure2Arg.
	self assert: block compiledBlock numTemps equals: 2.
	self assert: block compiledBlock numArgs equals: 2.
	self assert: block outerContext isNil.
	self assert: (block value: nil value: nil) equals: 1.
	self assert: (block valueWithArguments: #(2 2)) equals: 1.
	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch.

	self should: [block value] raise: ArgumentsCountMismatch.
	self should: [block value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch.

	block := [:arg1 :arg2 :arg3 | 1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block sourceNode isConstant.
	self assert: block class equals: ConstantBlockClosure3Arg.
	self assert: block compiledBlock numTemps equals: 3.
	self assert: block compiledBlock numArgs equals: 3.
	self assert: block outerContext isNil.
	self assert: (block value: nil value: nil value: nil) equals: 1.
	self assert: (block valueWithArguments: #(2 2 2)) equals: 1.

	self should: [block value] raise: ArgumentsCountMismatch.
	self should: [block value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch.

	block := [:arg1 :arg2 :arg3 :arg4 | 1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block sourceNode isConstant.
	"but we do not compile it as a ConstantBLockCloure for now"
	self assert: block class equals: CleanBlockClosure.
	self assert: block compiledBlock numTemps equals: 4.
	self assert: block compiledBlock numArgs equals: 4.

	self should: [block value] raise: ArgumentsCountMismatch.
	self should: [block value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch.


	block := [:arg1 :arg2 :arg3 :arg4 :arg5 | 1].
	self assert: block isClean.
	self deny: block hasPrimitive.
	self assert: block sourceNode isConstant.
	self assert: block class equals: CleanBlockClosure.
	self assert: block compiledBlock numTemps equals: 5.
	self assert: block compiledBlock numArgs equals: 5.

 	self should: [block value] raise: ArgumentsCountMismatch.
 	self should: [block value: nil] raise: ArgumentsCountMismatch.
 	self should: [block value: nil value: nil] raise: ArgumentsCountMismatch.
 	self should: [block value: nil value: nil value: nil] raise: ArgumentsCountMismatch.
 	self should: [block valueWithArguments: #()] raise: ArgumentsCountMismatch
]

{ #category : 'tests - empty' }
OCClosureTest >> testEmptyBlockOneArgument [
	self
		assert: (self class compiler evaluate: '[ :a ] value: 1') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ :a | ] value: 1') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ :a | | t | ] value: 1') isNil
		description: 'Empty blocks in ST-80 should return nil'
]

{ #category : 'tests - empty' }
OCClosureTest >> testEmptyBlockTwoArguments [
	self
		assert: (self class compiler evaluate: '[ :a :b ] value: 1 value: 2') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ :a :b | ] value: 1 value: 2') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ :a :b | | t | ] value: 1 value: 2') isNil
		description: 'Empty blocks in ST-80 should return nil'
]

{ #category : 'tests - empty' }
OCClosureTest >> testEmptyBlockZeroArguments [
	self
		assert: (self class compiler evaluate: '[ ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ | t | ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'
]

{ #category : 'tests' }
OCClosureTest >> testMethodArgument [
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	self assert: block value equals: 2
]

{ #category : 'tests' }
OCClosureTest >> testMethodTemp [
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoArgument [
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoArgumentNotInlined [
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoInsideTemp [
	1 to: 5 do: [ :index |
		| temp |
		temp := index.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoInsideTempNotInlined [
	| block |
	block := [ :index |
		| temp |
		temp := index.
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoOutsideTemp [
	| temp |
	1 to: 5 do: [ :index |
		temp := index.
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - todo' }
OCClosureTest >> testToDoOutsideTempNotInlined [
	| block temp |
	block := [ :index |
		temp := index.
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileModificationAfter [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		collection add: [ index ].
		index := index + 1 ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileModificationAfterNotInlined [
	| index block |
	index := 0.
	block := [
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileModificationBefore [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileModificationBeforeNotInlined [
	| index block |
	index := 0.
	block := [
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileWithTemp [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileWithTempIsNil [

	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		collection add: temp.
		temp := index := index + 1.
		collection add: temp].
	self assertValues: #(nil 1 nil 2 nil 3 nil 4 nil 5)
]

{ #category : 'tests - while' }
OCClosureTest >> testWhileWithTempNotInlined [
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(1 2 3 4 5)
]
