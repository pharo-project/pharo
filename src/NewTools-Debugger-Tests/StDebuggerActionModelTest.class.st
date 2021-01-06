Class {
	#name : #StDebuggerActionModelTest,
	#superclass : #TestCase,
	#instVars : [
		'session',
		'debugActionModel',
		'debugger',
		'result'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #benchmarking }
StDebuggerActionModelTest class >> benchFilterStack [
	<script>
	^self new benchmarkFilterStack inspect
]

{ #category : #benchmarks }
StDebuggerActionModelTest >> bench: aBlock [
	^ aBlock timeToRun asNanoSeconds
]

{ #category : #benchmarks }
StDebuggerActionModelTest >> benchmarkFilterStack [
	| stack1 stack2 beforeResults afterResults |
	[ self method1: 0 ]
		on: Halt
		do: [ :halt | 
			session := Process new
				           newDebugSessionNamed: 'profiling'
				           startedAt: halt signalerContext.
			stack1 := session stack.	
			stack2 := stack1 copy.		
			debugActionModel := StDebuggerActionModel new.
			debugActionModel session: session.
			beforeResults := 0.
			10000 timesRepeat: [ 
				beforeResults := beforeResults + (self bench: [ 
					                  stack1 reject: [ :aContext | 
						                  aContext method hasPragmaNamed:
							                  #debuggerCompleteToSender ] ]) ].
			afterResults := 0.
			10000 timesRepeat: [ 
				afterResults := afterResults
				                +
				                (self bench: [ 
					                 debugActionModel filterStack: stack2 ]) ] ].
	^'Old behavior: ', beforeResults printString, ' nanoseconds measured. 
	New behavior: ', afterResults printString, ' nanoseconds measured.'
]

{ #category : #helper }
StDebuggerActionModelTest >> changeSession: aDebugSession [

	session clear.
	session := aDebugSession.
	debugActionModel := StDebuggerActionModel on: session
]

{ #category : #helper }
StDebuggerActionModelTest >> emptyContextForMethod: aCompiledMethod [
	^ (Context newForMethod: aCompiledMethod)
		  initializeWith: nil
		  stackPtr: nil
		  method: aCompiledMethod
		  receiver: nil
		  sender: nil;
		  yourself
]

{ #category : #helper }
StDebuggerActionModelTest >> fileOutMethod: anObject [
	result := anObject
]

{ #category : #helper }
StDebuggerActionModelTest >> method1: i [
	^self method2: i+1
]

{ #category : #helper }
StDebuggerActionModelTest >> method2: i [
	i = 9999 ifTrue: [ self halt. ^i ].
	^self method1: i+1
]

{ #category : #helper }
StDebuggerActionModelTest >> methodClass [
	^self
]

{ #category : #helper }
StDebuggerActionModelTest >> methodWithPragma [
	<debuggerCompleteToSender>
]

{ #category : #helper }
StDebuggerActionModelTest >> session [
	^debugger session
]

{ #category : #helper }
StDebuggerActionModelTest >> setResult [
	result := 2 squared.
	^result
]

{ #category : #running }
StDebuggerActionModelTest >> setUp [
	| method context process |
	super setUp.
	method := self class >> #setResult.
	process := [ method valueWithReceiver: self arguments: #() ]
		newProcess.
	context := process suspendedContext.
	session ifNotNil: [ session clear ].
	session := (StTestDebuggerProvider new debuggerWithContext: context)
		session.
	session
		stepIntoUntil: [ :currentContext | currentContext method == method ].
	debugActionModel := StDebuggerActionModel on: session
]

{ #category : #running }
StDebuggerActionModelTest >> tearDown [
	debugger ifNotNil: [ debugger close].
	session ifNotNil: [ session clear ].
	session := nil.
	StDebuggerUsingSpecSelectorMock removeSelector: #testMethod.
	StDebuggerUsingSpecSelectorMock removeSelector: #method.
	StDebuggerUsingSpecSelectorMock removeSelector: #unclassifiedMethod.
	StTestDebuggerProvider removeSelector: #buildDebuggerWithMissingClassContext.
	StTestDebuggerProvider removeSelector: #foobar.
	super tearDown
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testClearDebugSession [
	| process |
	process := session interruptedProcess.
	self deny: process isTerminated.
	debugActionModel clearDebugSession.
	self assert: process isTerminated.
	self assert: session interruptedContext isNil
]

{ #category : #'tests - contexts' }
StDebuggerActionModelTest >> testComputeInitialTopContext [
	
	self changeSession: StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	self assert: debugActionModel topContext identicalTo: session exception signalContext.
	
	session := StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	session restart: session interruptedContext sender.	
	self changeSession: session.
	self assert: debugActionModel topContext identicalTo: session interruptedContext
	
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testCopyStackToClipboard [
	debugActionModel copyStackToClipboard.
	self
		assert: Clipboard clipboardText string
		equals:
			(String
				streamContents: [ :s | session interruptedContext shortDebugStackOn: s ])
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testCreateMissingMethod [
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testFileOutMethod [
	debugActionModel fileOutMethod: self.
	self assert: result equals: self selector
]

{ #category : #tests }
StDebuggerActionModelTest >> testFilterDNUStack [

	|stack filteredStack|
	self changeSession: StTestDebuggerProvider new debuggerWithDNUContext session.
	stack := OrderedCollection new.
	
	"First, the stack contains a top context (i.e. from which a signal was sent), then the interrupted context"
	stack add: debugActionModel topContext.
	
	"Second, the stack contexts is a MNU that has a method with the <debuggerCompleteToSender> pragma"
	stack add: debugActionModel interruptedContext.	
	
	"Then we have an imaginary call stack"
	stack add: (self emptyContextForMethod: (self class>>#method1:)).
	stack add: (self emptyContextForMethod: (self class>>#method2:)).
	
	filteredStack := debugActionModel filterStack: stack copy.
	
	self assert: filteredStack size equals: 2.
	self assert: filteredStack first identicalTo: stack third.
	self assert: filteredStack second identicalTo: stack fourth
	
]

{ #category : #tests }
StDebuggerActionModelTest >> testFilterMissingSubclassResponsibilityStack [

	|stack filteredStack|	
	
	"We change the stack with an interrupted context that does not contain the <debuggerCompleteToSender> pragma"
	self changeSession: StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	stack := OrderedCollection new.
	stack add: debugActionModel topContext.
	stack add: debugActionModel topContext sender.
	stack add: debugActionModel interruptedContext.
	stack add: (self emptyContextForMethod: (self class>>#method1:)).
	stack add: (self emptyContextForMethod: (self class>>#method2:)).
	
	filteredStack := debugActionModel filterStack: stack copy.
	self assert: filteredStack size equals: 2.
	self assert: filteredStack first identicalTo: stack fourth.
	self assert: filteredStack second identicalTo: stack fifth
]

{ #category : #tests }
StDebuggerActionModelTest >> testFilterStack [

	|stack filteredStack|
	
	stack := OrderedCollection new.
	
	"First, the stack contains the interrupted context"	
	stack add: debugActionModel interruptedContext.	
	
	"Then we have an imaginary call stack"
	stack add: (self emptyContextForMethod: (self class>>#method1:)).
	stack add: (self emptyContextForMethod: (self class>>#method2:)).
	
	filteredStack := debugActionModel filterStack: stack copy
]

{ #category : #tests }
StDebuggerActionModelTest >> testImplementClassifiedInClassForContext [
	|method|
	method := (StDebuggerUsingSpecSelectorMock lookupSelector: #method).
	self assert:	method equals: nil.
	[ StDebuggerUsingSpecSelectorMock new method ]
		on: Error
		do: [ :dnu | 
			debugActionModel
				implement: dnu message
				classified: Protocol unclassified
				inClass: StDebuggerUsingSpecSelectorMock
				forContext: dnu signalerContext ].
	method := (StDebuggerUsingSpecSelectorMock lookupSelector: #method).
	self deny: method	equals: nil.
	self assert: method methodClass identicalTo: StDebuggerUsingSpecSelectorMock
]

{ #category : #tests }
StDebuggerActionModelTest >> testImplementMethodClassification [

	| method |
	[ StDebuggerUsingSpecSelectorMock new method ]
		on: Error
		do: [ :dnu | 
			debugActionModel
				implement: dnu message
				classified: Protocol unclassified
				inClass: StDebuggerUsingSpecSelectorMock
				forContext: dnu signalerContext ].

	[ StDebuggerUsingSpecSelectorMock new testMethod ]
		on: Error
		do: [ :dnu | 
			debugActionModel
				implement: dnu message
				classified: Protocol unclassified
				inClass: StDebuggerUsingSpecSelectorMock
				forContext: dnu signalerContext ].

	[ StDebuggerUsingSpecSelectorMock new unclassifiedMethod ]
		on: Error
		do: [ :dnu | 
			debugActionModel
				implement: dnu message
				classified: Protocol unclassified
				inClass: StDebuggerUsingSpecSelectorMock
				forContext: dnu signalerContext ].

	method := StDebuggerUsingSpecSelectorMock lookupSelector: #method.
	self assert: method protocol equals: 'accessing'.
	
	method := StDebuggerUsingSpecSelectorMock lookupSelector: #testMethod.
	self assert: method protocol equals: 'tests'.
	
	method := StDebuggerUsingSpecSelectorMock lookupSelector: #unclassifiedMethod.
	self assert: method protocol equals: Protocol unclassified
]

{ #category : #tests }
StDebuggerActionModelTest >> testInitialization [
	self assert: debugActionModel session identicalTo: session.
	self
		assert: debugActionModel interruptedContext
		identicalTo: session interruptedContext
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsContextAnAssertionFailure [
	self changeSession: StTestDebuggerProvider new debuggerWithFailingAssertionContext session.
	self assert: debugActionModel isContextAnAssertionFailure
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsContextSteppable [
	self assert: debugActionModel isContextSteppable
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsInterruptedContextAnAssertEqualsFailure [

	self changeSession:
		StTestDebuggerProvider new debuggerWithFailingAssertionContext
			session.
	self assert: debugActionModel isContextAnAssertionFailure
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsInterruptedContextDoesNotUnderstand [
	self changeSession: StTestDebuggerProvider new debuggerWithDNUContext session.
	self assert: debugActionModel isInterruptedContextDoesNotUnderstand.
	StTestDebuggerProvider compile: 'foobar ^self'.
	self deny: debugActionModel isInterruptedContextDoesNotUnderstand
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsInterruptedContextMissingClassException [	
	StTestDebuggerProvider compileMissingClassContextBuilder.
	self changeSession: StTestDebuggerProvider new debuggerWithMissingClassContext session.
	self assert: debugActionModel isInterruptedContextMissingClassException
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsInterruptedContextPostMortem [
	session resume.
	self assert: debugActionModel isInterruptedContextPostMortem
]

{ #category : #'tests - predicates' }
StDebuggerActionModelTest >> testIsInterruptedContextSubclassResponsibilityException [
	self changeSession: StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	self assert: debugActionModel isInterruptedContextSubclassResponsibilityException
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testPeelToFirstLike [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testProceedDebugSession [
	self deny: session interruptedContext isNil.
	self assert: result isNil.
	debugActionModel proceedDebugSession.
	self assert: session interruptedContext isNil.
	self assert: result equals: 4
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testRestartContext [
	|topContext topPC|
	topContext := 
	session interruptedContext.
	topPC := topContext pc.
	session stepInto.
	self deny: session interruptedContext identicalTo: topContext.
	debugActionModel restartContext: topContext.
	self assert: session interruptedContext identicalTo: topContext.
	self assert: session interruptedContext pc equals: topPC
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testReturnValueFromExpressionFromContext [
	session stepInto.
	session stepInto.
	debugActionModel returnValueFromExpression: '0' fromContext: session interruptedContext.
	self assert: session interruptedContext top equals: 0.
	session resume.
	self assert: result equals: 0
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testRunToSelectionInContext [
	|method node|
	method := (self class >> #setResult).
	node :=  method ast statements second.
	debugActionModel runToSelection: (node start to: node stop) inContext: session interruptedContext.
	self assert: result equals: 4.
	self assert: (method sourceNodeForPC: session interruptedContext pc) identicalTo: node
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testStepInto [
	|ctx|
	ctx := session interruptedContext.
	debugActionModel stepInto: ctx.
	self assert: session interruptedContext sender identicalTo: ctx.
	self assert: session stack second identicalTo: ctx
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testStepOver [
	| method ctx |
	method := self class >> #setResult.
	ctx := session interruptedContext.
	debugActionModel stepOver: ctx.
	self assert: session interruptedContext identicalTo: ctx.
	self assert: ctx top equals: 4.
	debugActionModel stepOver: ctx.
	self assert: session interruptedContext identicalTo: ctx.
	self assert: result equals: 4.
	self
		assert: (method sourceNodeForPC: session interruptedContext pc)
		identicalTo: method ast statements second
]

{ #category : #'tests - actions' }
StDebuggerActionModelTest >> testStepThrough [
	| method ctx |
	method := self class >> #setResult.
	ctx := session interruptedContext.
	debugActionModel stepThrough: ctx.
	self assert: session interruptedContext identicalTo: ctx.
	self assert: ctx top equals: 4.
	debugActionModel stepThrough: ctx.
	self assert: session interruptedContext identicalTo: ctx.
	self assert: result equals: 4.
	self
		assert: (method sourceNodeForPC: session interruptedContext pc)
		identicalTo: method ast statements second
]

{ #category : #'tests - contexts' }
StDebuggerActionModelTest >> testUpdateTopContext [
	
	self changeSession: StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	self deny: debugActionModel topContext identicalTo: debugActionModel interruptedContext.
	debugActionModel updateTopContext.		
	self assert: debugActionModel topContext identicalTo: debugActionModel interruptedContext
	
	
	
]

{ #category : #'tests - contexts' }
StDebuggerActionModelTest >> testUpdateTopContextAfterSessionOperation [
	|mockDebugActionModel|
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel stepInto: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel stepOver: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel stepThrough: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel restartContext: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel returnValueFromExpression: 'nil' fromContext: [] asContext.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel runToSelection: nil inContext: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext.
	
	mockDebugActionModel := StMockDebuggerActionModel new.
	mockDebugActionModel implement: nil classified: nil inClass: nil forContext: nil.
	self assert: mockDebugActionModel tag equals: #updateTopContext
	
	
	
]
