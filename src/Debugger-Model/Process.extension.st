Extension { #name : 'Process' }

{ #category : '*Debugger-Model' }
Process >> activateReturn: aContext value: value [
	"Activate 'aContext return: value', so execution will return to aContext's sender"

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext activateReturn: aContext value: value]
		onBehalfOf: self
]

{ #category : '*Debugger-Model' }
Process >> completeStep: aContext [
	"Resume self until aContext is on top, or if already on top, complete next step"

	| callee |
	self suspendedContext == aContext ifFalse: [
		^ self complete: (self calleeOf: aContext)].
	callee := self step.
	callee == aContext ifTrue: [^ callee].
	aContext isDead ifTrue: [^ self suspendedContext].  "returned"
	^ self complete: callee  "finish send"
]

{ #category : '*Debugger-Model' }
Process >> newDebugSessionNamed: aString startedAt: aContext [

	^DebugSession named: aString on: self startedAt: aContext
]

{ #category : '*Debugger-Model' }
Process >> popTo: aContext [
	"Pop self down to aContext by remote returning from aContext's callee.  Unwind blocks will be executed on the way.
	This is done by pushing a new context on top which executes 'aContext callee return' then resuming self until aContext is reached.  This way any errors raised in an unwind block will get handled by senders in self and not by senders in the activeProcess.
	If an unwind block raises an error that is not handled then the popping stops at the error and the signalling context is returned, othewise aContext is returned."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: callee receiver]
				onBehalfOf: self]
]

{ #category : '*Debugger-Model' }
Process >> popTo: aContext value: aValue [
	"Replace the suspendedContext with aContext, releasing all contexts
	 between the currently suspendedContext and it."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: aValue]
				onBehalfOf: self]
]

{ #category : '*Debugger-Model' }
Process >> restartTop [
	"Rollback top context and replace with new method.  Assumes self is suspended"

	suspendedContext privRefresh
]

{ #category : '*Debugger-Model' }
Process >> restartTopWith: method [
	"Rollback top context and replace with new method.  Assumes self is suspended"

	suspendedContext privRefreshWith: method
]

{ #category : '*Debugger-Model' }
Process >> return: aContext value: value [
	"Pop thread down to aContext's sender.  Execute any unwind blocks on the way.  See #popTo: comment and #runUntilErrorOrReturnFrom: for more details."

	suspendedContext == aContext ifTrue:
		[^Processor activeProcess
			evaluate: [suspendedContext := aContext return: value from: aContext]
			onBehalfOf: self].
	self activateReturn: aContext value: value.
	^self complete: aContext
]

{ #category : '*Debugger-Model' }
Process >> stepToHome: aContext [
	| ctxt pair error |
	ctxt := suspendedContext.
	suspendedContext := nil.
	pair := Processor activeProcess
				evaluate: [ctxt stepToHome: aContext]
				onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.

	error ifNotNil: [
		suspendedContext := error signalerContext.
		"As we are activating a context that has been interrupted in the signal of the exception,
		we need to push a receiver of the signal message.
		A suspended context should always be a top context.
		A top context has the return value of the message in the stack.
		As this context has been suspended while sending a message the return value should be pushed.
		This is maybe not the expected return value (the #signal message returns the value with the
		one the exception is resumed).
		But this allows the debugger to continue executing and does not crash the interpreter nor
		the VM
		"
		suspendedContext push: nil].
	^ suspendedContext
]
