Class {
	#name : #ContextDebuggingTest,
	#superclass : #TestCase,
	#category : #'Debugging-Utils-Tests'
}

{ #category : #tests }
ContextDebuggingTest >> deadContext [
	| process context |
	process := [  ] newProcess.
	context := process suspendedContext.
	context pc: nil.
	^ context
]

{ #category : #tests }
ContextDebuggingTest >> selfReturnMethod [
	#foo asSymbol 
]

{ #category : #tests }
ContextDebuggingTest >> testRestartQuickMethod [

	|aReceiver aMethodContext newContext stackSize|
	aReceiver := 100@100 corner: 200@200.
	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: (Rectangle >>#center)
		arguments: #().
	
	"We step into #center to get on the topLeft message.
	We did not change context."
	aMethodContext step.
	
	"Now we step into #topLeft which is a quick method"
	aMethodContext stepIntoQuickMethod: true.	
	"We simulate a first step and we save the stack size"
	newContext := aMethodContext step.
	stackSize := newContext stack size.
	"Now we simulate a restart of the context: the context is refreshed (pc reset) and
	stepped to the first interesting bytecode (a send or a return)."
	newContext privRefresh.
	newContext := newContext stepToSendOrReturn.
	
	"The stack should be preserved"
	self assert: newContext stack size equals: stackSize.
	self assert: newContext stack first method identicalTo: (Rectangle >> #topLeft).
	self assert: newContext stack second method identicalTo: (Rectangle >>#center)
]

{ #category : #tests }
ContextDebuggingTest >> testStepToSendOrReturn [
	| context |
	context := self deadContext.
	self assert: context isDead.
	context := context stepToSendOrReturn.
	self assert: context isDead
]

{ #category : #tests }
ContextDebuggingTest >> testStepToSendOrReturnIntoReturnSelfQuickMethod [

	|aMethodContext newContext|
	aMethodContext := Context
		sender: thisContext
		receiver: self class basicNew
		method: (self class >> #selfReturnMethod)
		arguments: #().

	newContext := aMethodContext step.
	aMethodContext stepIntoQuickMethod: true.
	newContext := newContext step.
	newContext := newContext stepToSendOrReturn. 	
	
	"Stepping to end or return of a quick method just returning self
	puts the pc at the return of the quick method, if it is not already there."
	self assert: newContext pc equals: (Symbol >> #asSymbol) endPC.
]
