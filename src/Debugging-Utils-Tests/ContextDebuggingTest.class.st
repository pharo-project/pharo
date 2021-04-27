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
