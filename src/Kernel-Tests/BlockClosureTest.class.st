"
SUnit tests for BlockClosure
"
Class {
	#name : #BlockClosureTest,
	#superclass : #TestCase,
	#instVars : [
		'aBlockContext',
		'contextOfaBlockContext'
	],
	#category : #Kernel-Tests-Methods
}

{ #category : #helpers }
BlockClosureTest >> blockWithNonLocalReturn: resultObject [
		
	^[ ^resultObject ] 
]

{ #category : #running }
BlockClosureTest >> setUp [
	super setUp.
	aBlockContext := [100@100 corner: 200@200].
	contextOfaBlockContext := thisContext.
]

{ #category : #'tests - evaluating' }
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

{ #category : #'tests - evaluating' }
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

{ #category : #'tests - evaluating' }
BlockClosureTest >> testCull [
	[  ] cull: 1.
	[ :x |  ] cull: 1.
	self should: [ [ :x :y |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z :a |  ] cull: 1 ] raise: Error.
	self should: [ [ :x :y :z :a :b |  ] cull: 1 ] raise: Error.
	self assert: ([ 0 ] cull: 1) = 0.
	self assert: ([ :x | x ] cull: 1) = 1
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testCullCull [
	[  ] cull: 1 cull: 2.
	[ :x |  ] cull: 1 cull: 2.
	[ :x :y |  ] cull: 1 cull: 2.
	self should: [ [ :x :y :z |  ] cull: 1 cull: 2 ] raise: Error.
	self should: [ [ :x :y :z :a |  ] cull: 1 cull: 2 ] raise: Error.
	self should: [ [ :x :y :z :a :b |  ] cull: 1 cull: 2 ] raise: Error.
	self assert: ([ 0 ] cull: 1 cull: 2) = 0.
	self assert: ([ :x | x ] cull: 1 cull: 2) = 1.
	self assert: ([ :x :y | y ] cull: 1 cull: 2) = 2
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testCullCullCull [
	[  ] cull: 1 cull: 2 cull: 3.
	[ :x |  ] cull: 1 cull: 2 cull: 3.
	[ :x :y |  ] cull: 1 cull: 2 cull: 3.
	[ :x :y :z |  ] cull: 1 cull: 2 cull: 3.
	self should: [ [ :x :y :z :a |  ] cull: 1 cull: 2 cull: 3 ] raise: Error.
	self should: [ [ :x :y :z :a :b |  ] cull: 1 cull: 2 cull: 3 ] raise: Error.
	self assert: ([ 0 ] cull: 1 cull: 2 cull: 3) = 0.
	self assert: ([ :x | x ] cull: 1 cull: 2 cull: 3) = 1.
	self assert: ([ :x :y | y ] cull: 1 cull: 2 cull: 3) = 2.
	self assert: ([ :x :y :z | z ] cull: 1 cull: 2 cull: 3) = 3
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testCullCullCullCull [
	[  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y :z |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	[ :x :y :z :a |  ]
		cull: 1
		cull: 2
		cull: 3
		cull: 4.
	self
		should: [ 
			[ :x :y :z :a :b |  ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4 ]
		raise: Error.
	self
		assert:
			([ 0 ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 0.
	self
		assert:
			([ :x | x ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 1.
	self
		assert:
			([ :x :y | y ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 2.
	self
		assert:
			([ :x :y :z | z ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 3.
	self
		assert:
			([ :x :y :z :a | a ]
				cull: 1
				cull: 2
				cull: 3
				cull: 4) = 4
]

{ #category : #tests }
BlockClosureTest >> testNew [
	self	should: [Context new: 5] raise: Error.
	[Context new: 5]
		ifError: [:error | error = 'Error: Contexts must only be created with newForMethod:'].
	[Context new]
		ifError: [:error  | error = 'Error: Contexts must only be created with newForMethod:'].	
	[Context basicNew]
		ifError: [:error | error = 'Error: Contexts must only be created with newForMethod:'].				


]

{ #category : #tests }
BlockClosureTest >> testNoArguments [
	[10
		timesRepeat: [:arg | 1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 1 arguments.'].
	[10
		timesRepeat: [:arg1 :arg2 | 1 + 2]]
		ifError: [:err  | self deny: err = 'This block requires 2 arguments.'] 
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnFork [
	"Test that if code runs without errors, there is no fork! "
	
	| result1 result2 |
	
	result2 := nil.
	result1 := [ 1 ] on: Exception fork: [ result2 := 2 ].
	
	Processor yield.
	
	self assert: (result1 = 1).
	self assert: (result2 isNil ).
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnForkErrorExecutesBlock [
	"Test that if code runs with error, there is fork"
	
	| result sema |
	sema := Semaphore new.	
	result := nil.
	[ 1/0 ] on: Exception fork: [ result := 2. sema signal].
		
	sema wait.	
	"and of course result should be not nil "
	self assert: result = 2.
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnForkErrorOnSeparateProcess [
	"Test that if code runs with error, there is fork"
	
	| result forkedProc sema |
	
	sema := Semaphore new.	
	result := [ 1/0 ] on: Exception fork: [ forkedProc := Processor activeProcess. sema signal ].

	sema wait.
	self assert: (forkedProc ~~ Processor activeProcess).
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnForkErrorReturnsNil [
	"Test that if code runs with error, there is fork"
	
	| result sema |
	
	sema := Semaphore new.	
	result := [ 1/0 ] on: Exception fork: [ sema signal. ].
	
	sema wait.
	"in case of error, evaluation result should be nil"
	self assert: result isNil.
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnForkErrorTakesLessThanOneSecond [
	"Test that if code runs with error, there is fork"
	
	| sema timeout |

	self flag: 'This test is too brittle, failing often on Windows CI'; skip.
		
	self flag: 'The following line makes the test pass under headless linux. Everywhere else this test works'.
	Smalltalk os isUnix
		ifTrue: [ 1 milliSecond wait ].
	
	sema := Semaphore new.
	[ 1/0 ] on: Exception fork: [ sema signal ].
		
	timeout := (sema waitTimeoutSeconds: 1).	
	self assert: timeout == false
]

{ #category : #'tests - on-fork' }
BlockClosureTest >> testOnForkSplit [
	"Test that when forking, the stack are split correctly (there is no any contexts referenced by both processes)"
	
	| sema timeout forkedContexts myContexts c |
	
	self flag: 'This test is too brittle, failing often on CI'; skip.
		
	sema := Semaphore new.
	
	[ 1/0 ] on: Exception fork: [  | ctx |
		forkedContexts := IdentitySet new.
		ctx := thisContext.
		[ ctx notNil ] whileTrue: [ forkedContexts add: ctx. ctx := ctx sender ].
		sema signal ].
	
	timeout := (sema waitTimeoutSeconds: 1).
	self assert: timeout == false description: 'fork lasted more than one second'.

	myContexts := IdentitySet new.
	c := thisContext.
	[ c notNil ] whileTrue: [ myContexts add: c. c := c sender ].

	self assert: (myContexts noneSatisfy: [:b | forkedContexts includes: b ]) description: 'myContexts are not within forkedContexts'.
	self assert: (forkedContexts noneSatisfy: [:b | myContexts includes: b ]) description: 'forkedContexts are not within myContexts'.
]

{ #category : #tests }
BlockClosureTest >> testOneArgument [
	| c |
	c := OrderedCollection new.
	c add: 'hello'.
	[c
		do: [1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 0 arguments.'].
	[c
		do: [:arg1 :arg2 | 1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 2 arguments.'] 
]

{ #category : #tests }
BlockClosureTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlockContext) class = Rectangle.
]

{ #category : #tests }
BlockClosureTest >> testSetUp [
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"

	self assert: aBlockContext home = contextOfaBlockContext.
	self assert: aBlockContext receiver = self.
	"Depending on the closure implementation, it's either a compiled block, a compiled method or nil."
	self assert: (aBlockContext method isNil or: [aBlockContext method isKindOf: CompiledCode]).
]

{ #category : #'tests - astmapping' }
BlockClosureTest >> testSourceNodeOptimized [
	| block | 
	block := [ :ctx | [ ctx atEnd ] whileTrue:[1+2 ] ] .
	self assert: block sourceNode printString = 'RBBlockNode([ :ctx | [ ctx atEnd ] whileTrue: [ 1 + 2 ] ])'.

]

{ #category : #testing }
BlockClosureTest >> testSupplyAnswerOfFillInTheBlank [

	self should: ['blue' = ([UIManager default request: 'Your favorite color?'] 
		valueSupplyingAnswer: #('Your favorite color?' 'blue'))]
]

{ #category : #testing }
BlockClosureTest >> testSupplyAnswerOfFillInTheBlankUsingDefaultAnswer [

	self should: ['red' = ([UIManager default  request: 'Your favorite color?' initialAnswer: 'red'] 
		valueSupplyingAnswer: #('Your favorite color?' #default))]
]

{ #category : #tests }
BlockClosureTest >> testSupplyAnswerThroughNestedBlocks [

	self should: [true = ([[self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('Blub' false)] valueSupplyingAnswer: #('Smalltalk' true))]
]

{ #category : #tests }
BlockClosureTest >> testSupplyAnswerUsingOnlySubstringOfQuestion [

	self should: [false = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('like' false))]
]

{ #category : #tests }
BlockClosureTest >> testSupplyAnswerUsingRegexMatchOfQuestion [

	(String includesSelector: #matchesRegex:) ifFalse: [^ self].
	
	self should: [true = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('.*Smalltalk\?' true))]
]

{ #category : #tests }
BlockClosureTest >> testSupplyAnswerUsingTraditionalMatchOfQuestion [

	self should: [true = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('*Smalltalk#' true))]
]

{ #category : #tests }
BlockClosureTest >> testSupplySameAnswerToAllQuestions [

	self should: [true = ([self confirm: 'You like Smalltalk?'] valueSupplyingAnswer: true)].
	
	self should: [#(true true) = ([{self confirm: 'One'. self confirm: 'Two'}] valueSupplyingAnswer: true)].
]

{ #category : #tests }
BlockClosureTest >> testSupplySeveralAnswersToSeveralQuestions [

	self should: [#(false true) = ([{self confirm: 'One'. self confirm: 'Two'}] 
		valueSupplyingAnswers: #( ('One' false) ('Two' true) ))].
	
	self should: [#(true false) = ([{self confirm: 'One'. self confirm: 'Two'}] 
		valueSupplyingAnswers: #( ('One' true) ('Two' false) ))]
]

{ #category : #tests }
BlockClosureTest >> testSupplySpecificAnswerToQuestion [

	self should: [false = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('You like Smalltalk?' false))]
]

{ #category : #tests }
BlockClosureTest >> testSuppressInform [

	self should: [[nil inform: 'Should not see this message or this test failed!'] valueSuppressingAllMessages isNil]
]

{ #category : #tests }
BlockClosureTest >> testSuppressInformUsingStringMatchOptions [

	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('Should not see this message or this test failed!')) isNil].
	
	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('not see this message')) isNil].
	
	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('*message*failed#')) isNil].

]

{ #category : #tests }
BlockClosureTest >> testTallyInstructions [
	"Depends on compiler settings... If everything is compiled to the V3PlusClosures bytecode set, then the result is 27.
	 If everything is compiled to the SistaV1 bytecode set, then the result is 26. If execution is mixed between method
	compiled with V3PlusClosures and SistaV1, result is then different and untested."
	
	"We test class name and not the class because there are multiple versions of the encoders depending on the compiler used."
	| expectedResult |
	expectedResult := 27.
	self assert: (Context tallyInstructions: aBlockContext ) size equals: expectedResult
]

{ #category : #tests }
BlockClosureTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlockContext) size = 7.
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testValueWithArguments [
	self should: [ aBlockContext valueWithArguments: #(1) ] raise: Error.
	aBlockContext valueWithArguments: #().
	[ aBlockContext valueWithArguments: #(1) ]
		ifError: [ :err | self assert: err = 'Error: This block accepts 0 arguments, but was called with 1 argument.' ].
	[ [ :i | 3 + 4 ] valueWithArguments: #(1 2) ]
		ifError: [ :err | self assert: err = 'Error: This block accepts 1 argument, but was called with 2 arguments.' ]
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testValueWithExitBreak [

	| val |	

	[ :break |
	    1 to: 10 do: [ :i |
			val := i.
			i = 4 ifTrue: [break value].
		] 
	] valueWithExit.

	self assert: val = 4.
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testValueWithExitContinue [

	| val last |	
	val := 0. 

	1 to: 10 do: [ :i |
		[ :continue |
			i = 4 ifTrue: [continue value].
			val := val + 1.
			last := i
		] valueWithExit.
	].

	self assert: val = 9.
	self assert: last = 10.	
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testValueWithPossibleArgs [
	| block blockWithArg blockWith2Arg |
	block := [ 1 ].
	blockWithArg := [ :arg | arg ].
	blockWith2Arg := [ :arg1 :arg2 | 
	{arg1.
	arg2} ].
	self assert: (block valueWithPossibleArgs: #()) = 1.
	self assert: (block valueWithPossibleArgs: #(1)) = 1.
	self assert: (blockWithArg valueWithPossibleArgs: #()) isNil.
	self assert: (blockWithArg valueWithPossibleArgs: #(1)) = 1.
	self assert: (blockWithArg valueWithPossibleArgs: #(1 2)) = 1.
	self
		assert:
			(blockWith2Arg valueWithPossibleArgs: #())
				=
					{nil.
					nil}.
	self
		assert:
			(blockWith2Arg valueWithPossibleArgs: #(1))
				=
					{1.
					nil}.
	self assert: (blockWith2Arg valueWithPossibleArgs: #(1 2)) = #(1 2).
	self assert: (blockWith2Arg valueWithPossibleArgs: #(1 2 3)) = #(1 2)
]

{ #category : #'tests - evaluating' }
BlockClosureTest >> testValueWithPossibleArgument [
	| block  blockWithArg blockWith2Arg |

	block := [1].
	blockWithArg  := [:arg | arg].
	blockWith2Arg := [:arg1 :arg2 | {arg1. arg2}].

	self assert: (block valueWithPossibleArgument: 1) = 1.
	
	self assert: (blockWithArg valueWithPossibleArgument: 1) = 1.
	
	self assert: (blockWith2Arg valueWithPossibleArgument: 1) = {1 . nil}.
	

	
]
