Extension { #name : #BlockClosureTest }

{ #category : #'*Kernel-Tests-Extended' }
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

{ #category : #'*Kernel-Tests-Extended' }
BlockClosureTest >> testCannotReturn [

	| block p |
	p := [block := self blockWithNonLocalReturn: #result] fork.
	[p isTerminated] whileFalse: [ 10 milliSeconds wait ].
 
	[ block value.	
	self assert: false description: 'BlockCannotReturn should be signaled' ]
		 on: BlockCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err home equals: block home]
]

{ #category : #'*Kernel-Tests-Extended' }
BlockClosureTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlockContext) class equals: Rectangle
]

{ #category : #'*Kernel-Tests-Extended' }
BlockClosureTest >> testSourceNodeOptimized [
	| block |
	block := [ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ].
	self assert: block sourceNode printString equals: 'RBBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'
]

{ #category : #'*Kernel-Tests-Extended' }
BlockClosureTest >> testTallyInstructions [
	"Depends on compiler settings... If everything is compiled to the V3PlusClosures bytecode set, then the result is 27.
	 If everything is compiled to the SistaV1 bytecode set, then the result is 26. If execution is mixed between method
	compiled with V3PlusClosures and SistaV1, result is then different and untested."
	
	"We test class name and not the class because there are multiple versions of the encoders depending on the compiler used."
	| expectedResult |
	expectedResult := (aBlockContext method encoderClass = EncoderForSistaV1) ifTrue: [26] ifFalse: [27].
	self assert: (Context tallyInstructions: aBlockContext ) size equals: expectedResult
]

{ #category : #'*Kernel-Tests-Extended' }
BlockClosureTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlockContext) size equals: 7
]
