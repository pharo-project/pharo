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

	session terminate.
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
	session := (StTestDebuggerProvider new debuggerWithContext: context)
		session.
	session
		stepIntoUntil: [ :currentContext | currentContext method == method ].
	debugActionModel := StDebuggerActionModel on: session
]

{ #category : #running }
StDebuggerActionModelTest >> tearDown [
	session interruptedContext ifNotNil: [ session terminate ].
	(StDebuggerUsingSpecSelectorMock lookupSelector: #testMethod) 
		ifNotNil: [ :method | method removeFromSystem ].
	(StTestDebuggerProvider lookupSelector:
		 #buildDebuggerWithMissingClassContext) ifNotNil: [ :method | 
		method removeFromSystem ].
	super tearDown
]

{ #category : #tests }
StDebuggerActionModelTest >> testClearDebugSession [
	| process |
	process := session interruptedProcess.
	self deny: process isTerminated.
	debugActionModel clearDebugSession.
	self assert: process isTerminated.
	self assert: session interruptedContext isNil
]

{ #category : #tests }
StDebuggerActionModelTest >> testCopyStackToClipboard [
	debugActionModel copyStackToClipboard.
	self
		assert: Clipboard clipboardText string
		equals:
			(String
				streamContents: [ :s | session interruptedContext shortDebugStackOn: s ])
]

{ #category : #tests }
StDebuggerActionModelTest >> testCreateMissingMethod [
]

{ #category : #tests }
StDebuggerActionModelTest >> testExceptionInContextOrNil [
	|exOrNil|	
	StTestDebuggerProvider compileMissingClassContextBuilder.
	exOrNil := debugActionModel exceptionInContextOrNil: session interruptedContext. 
	self assert: exOrNil equals: nil.
	
	self
		changeSession: StTestDebuggerProvider new debuggerWithMissingClassContext session.		
	exOrNil := debugActionModel exceptionInContextOrNil: session interruptedContext. 
	self assert: exOrNil class identicalTo: VariableNotDeclared. 
]

{ #category : #tests }
StDebuggerActionModelTest >> testFileOutMethod [
	debugActionModel fileOutMethod: self.
	self assert: result equals: self selector
]

{ #category : #tests }
StDebuggerActionModelTest >> testFilterStack [

	|stack filteredStack|
	stack := OrderedCollection new.
	"First context references a method with the <debuggerCompleteToSender> pragma"
	stack add: (self emptyContextForMethod: (self class>>#methodWithPragma)).
	stack add: (self emptyContextForMethod: (self class>>#method1:)).
	stack add: (self emptyContextForMethod: (self class>>#method2:)).
	
	filteredStack := debugActionModel filterStack: stack copy.
	
	self assert: filteredStack size equals: 2.
	self assert: filteredStack first identicalTo: stack second.
	self assert: filteredStack second identicalTo: stack third.
	
	stack := OrderedCollection new.
	stack add: (self emptyContextForMethod: (self class>>#method1:)).
	stack add: (self emptyContextForMethod: (self class>>#method2:)).
	
	filteredStack := debugActionModel filterStack: stack copy.
	self assertCollection: filteredStack equals: stack 
]

{ #category : #tests }
StDebuggerActionModelTest >> testImplementClassifiedInClassForContext [
	|method|
	method := (StDebuggerUsingSpecSelectorMock lookupSelector: #testMethod).
	self assert:	method equals: nil.
	[ StDebuggerUsingSpecSelectorMock new testMethod ]
		on: Error
		do: [ :dnu | 
			debugActionModel
				implement: dnu message
				classified: Protocol unclassified
				inClass: StDebuggerUsingSpecSelectorMock
				forContext: dnu signalerContext ].
	method := (StDebuggerUsingSpecSelectorMock lookupSelector: #testMethod).
	self deny: method	equals: nil.
	self assert: method methodClass identicalTo: StDebuggerUsingSpecSelectorMock.
	self assert: method protocol equals: Protocol unclassified
]

{ #category : #tests }
StDebuggerActionModelTest >> testInitialization [
	self assert: debugActionModel session identicalTo: session.
	self
		assert: debugActionModel interruptedContext
		identicalTo: session interruptedContext
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsContextSteppable [
	self assert: debugActionModel isContextSteppable
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextATestFailure [
	self changeSession: StTestDebuggerProvider new debuggerWithFailingTestContext session.
	self assert: debugActionModel isInterruptedContextATestFailure
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextAnAssertEqualsFailure [
	self changeSession: StTestDebuggerProvider new debuggerWithFailingAssertionContext session.
	self assert: debugActionModel isInterruptedContextAnAssertEqualsFailure.
	self changeSession: StTestDebuggerProvider new debuggerWithFailingTestContext session.
	self deny: debugActionModel isInterruptedContextAnAssertEqualsFailure.
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextAnAssertionFailure [
	self changeSession: StTestDebuggerProvider new debuggerWithFailingAssertionContext session.
	self assert: debugActionModel isInterruptedContextATestFailure
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextDoesNotUnderstand [
	self changeSession: StTestDebuggerProvider new debuggerWithDNUContext session.
	self assert: debugActionModel isInterruptedContextDoesNotUnderstand
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextError [
	self skip.
	self flag: #DBG_TEST.
	self changeSession: StTestDebuggerProvider new debuggerWithErrorContext session.
	self assert: debugActionModel isInterruptedContextError
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextMissingClassException [	
	StTestDebuggerProvider compileMissingClassContextBuilder.
	self changeSession: StTestDebuggerProvider new debuggerWithMissingClassContext session.
	self assert: debugActionModel isInterruptedContextMissingClassException
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextPostMortem [
	session resume.
	self assert: debugActionModel isInterruptedContextPostMortem
]

{ #category : #tests }
StDebuggerActionModelTest >> testIsInterruptedContextSubclassResponsibilityException [
	self changeSession: StTestDebuggerProvider new debuggerWithMissingSubclassResponsibilityContext session.
	self assert: debugActionModel isInterruptedContextSubclassResponsibilityException
]

{ #category : #tests }
StDebuggerActionModelTest >> testPeelToFirstLike [
	self skip.
	self flag: #DBG_TEST
]

{ #category : #tests }
StDebuggerActionModelTest >> testProceedDebugSession [
	self deny: session interruptedContext isNil.
	self assert: result isNil.
	debugActionModel proceedDebugSession.
	self assert: session interruptedContext isNil.
	self assert: result equals: 4
]

{ #category : #tests }
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

{ #category : #tests }
StDebuggerActionModelTest >> testReturnValueFromExpressionFromContext [
	session stepInto.
	session stepInto.
	debugActionModel returnValueFromExpression: '0' fromContext: session interruptedContext.
	self assert: session interruptedContext top equals: 0.
	session resume.
	self assert: result equals: 0
]

{ #category : #tests }
StDebuggerActionModelTest >> testRunToSelectionInContext [
	|method node|
	method := (self class >> #setResult).
	node :=  method ast statements second.
	debugActionModel runToSelection: (node start to: node stop) inContext: session interruptedContext.
	self assert: result equals: 4.
	self assert: (method sourceNodeForPC: session interruptedContext pc) identicalTo: node
]

{ #category : #tests }
StDebuggerActionModelTest >> testStepInto [
	|ctx|
	ctx := session interruptedContext.
	debugActionModel stepInto: ctx.
	self assert: session interruptedContext sender identicalTo: ctx.
	self assert: session stack second identicalTo: ctx
]

{ #category : #tests }
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

{ #category : #tests }
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
