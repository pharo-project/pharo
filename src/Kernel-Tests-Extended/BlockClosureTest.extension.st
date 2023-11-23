Extension { #name : 'BlockClosureTest' }

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testBenchFor [
	| benchmarkResult duration minimumIterations |
	duration := 500 milliSeconds.
	benchmarkResult := [ 100 factorial ] benchFor: duration.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult elapsedTime >= duration.
	"These should pass even on a very slow machine or simulation"
	minimumIterations := 10.
	self assert: benchmarkResult iterations > minimumIterations.
	self assert: benchmarkResult period < (duration / minimumIterations)
]

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testBenchForException [

	"benchFor: expects a duration. Check that the bencher does not go in an infinite loop if the wakeup thread fails."

	self should: [ [ 100 factorial ] benchFor: 1 ] raise: Error
]

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testCannotReturn [

	| block p |
	p := [ block := self blockWithNonLocalReturn: #result ] fork.
	[ p isTerminated ] whileFalse: [ 10 milliSeconds wait ].

	self should: [ block value ] raise: BlockCannotReturn withExceptionDo: [ :err |
		self assert: err result equals: #result.
		self assert: err home equals: block home ]
]

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlock) class equals: Rectangle
]

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testSourceNodeOptimized [
	| block |
	block := [ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ].
	self assert: block sourceNode printString equals: 'RBBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'
]

{ #category : '*Kernel-Tests-Extended' }
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

{ #category : '*Kernel-Tests-Extended' }
BlockClosureTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlock) size equals: 7
]
