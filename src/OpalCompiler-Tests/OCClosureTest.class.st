Class {
	#name : #OCClosureTest,
	#superclass : #TestCase,
	#instVars : [
		'collection'
	],
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #utilities }
OCClosureTest >> assertValues: anArray [
	| values |
	values := collection collect: [ :each | each value ].
	self 
		assert: anArray asArray = values asArray
		description: ['Expected: ' , anArray asArray printString , 
			', but got ' , values asArray printString]
]

{ #category : #testing }
OCClosureTest >> methodArgument: anObject [
	^ [ anObject ]
	
]

{ #category : #running }
OCClosureTest >> setUp [
	super setUp.
	collection := OrderedCollection new
]

{ #category : #testing }
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

{ #category : #testing }
OCClosureTest >> testBlockTemp [
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : #testing }
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

{ #category : #'testing-empty' }
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

{ #category : #'testing-empty' }
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

{ #category : #'testing-empty' }
OCClosureTest >> testEmptyBlockZeroArguments [
	self
		assert: (self class compiler evaluate: '[ ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ | t | ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'
]

{ #category : #testing }
OCClosureTest >> testIsClean [
	 | local |
	 local := #testIsClean.
	self assert: [] isClean. "closes over nothing at all"
	self assert: [:a :b| a < b] isClean. "accesses only arguments"
	self assert: [:a :b| | s | s := a + b. s even] isClean. "accesses only local variables"
	self deny: [^nil] isClean. "closes over home (^-return)"
	self deny: [self] isClean. "closes over the receiver"
	self deny: [collection] isClean. "closes over the receiver (to access the inst var collection)"
	self deny: [local] isClean. "closes over local variable of outer context"
]

{ #category : #testing }
OCClosureTest >> testMethodArgument [
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	self assert: block value equals: 2
]

{ #category : #testing }
OCClosureTest >> testMethodTemp [
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value equals: 1.
	self assert: block2 value equals: 2
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoArgument [
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoArgumentNotInlined [
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoInsideTemp [
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoInsideTempNotInlined [
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoOutsideTemp [
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-todo' }
OCClosureTest >> testToDoOutsideTempNotInlined [
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-while' }
OCClosureTest >> testWhileModificationAfter [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		collection add: [ index ].
		index := index + 1 ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-while' }
OCClosureTest >> testWhileModificationAfterNotInlined [
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-while' }
OCClosureTest >> testWhileModificationBefore [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-while' }
OCClosureTest >> testWhileModificationBeforeNotInlined [
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : #'testing-while' }
OCClosureTest >> testWhileWithTemp [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)
]

{ #category : #'testing-while' }
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

{ #category : #'testing-while' }
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
