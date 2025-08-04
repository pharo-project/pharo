Extension { #name : 'BlockClosureTest' }

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testCannotReturn [

	| block p |
	p := [ block := self blockWithNonLocalReturn: #result ] fork.
	[ p isTerminated ] whileFalse: [ (Delay forMilliseconds: 10) wait ].

	self should: [ block value ] raise: BlockCannotReturn withExceptionDo: [ :err |
		self assert: err result equals: #result.
		self assert: err home equals: block home ]
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlock) class equals: Rectangle
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testSourceNodeOptimized [
	| block |
	block := [ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ].
	self assert: block sourceNode printString equals: 'OCBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testTallyInstructions [

	| expected |
	expected := Smalltalk compiler compilationContext
		            optionCleanBlockClosure
		            ifTrue: [ 26 ]
		            ifFalse: [ 27 ].
	self
		assert: (Context tallyInstructions: aBlock) size
		equals: expected
]

{ #category : '*Kernel-Extended-Tests' }
BlockClosureTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlock) size equals: 7
]
