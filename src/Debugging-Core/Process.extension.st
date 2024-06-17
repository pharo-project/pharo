Extension { #name : 'Process' }

{ #category : '*Debugging-Core' }
Process >> complete: aContext [
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signal context (rather than open a debugger)."

	| ctxt pair |
	ctxt := suspendedContext.
	suspendedContext := nil.	"disable this process while running its stack in active process below"
	pair := Processor activeProcess
				evaluate: [ ctxt runUntilErrorOrReturnFrom: aContext ]
				onBehalfOf: self.
	suspendedContext := pair first.
	^ pair second
		ifNil: [ suspendedContext ]
		ifNotNil: [ :error | error completeProcess: self with: aContext ]
]

{ #category : '*Debugging-Core' }
Process >> debug [
	^ self debugWithTitle: 'Debug'
]

{ #category : '*Debugging-Core' }
Process >> debugWithTitle: title [

	| context |
	context := self isActiveProcess
		           ifTrue: [ thisContext ]
		           ifFalse: [ self suspendedContext ].
	UIManager default
		requestDebuggerOpeningForProcess: self
		named: title
		inContext: context
]

{ #category : '*Debugging-Core' }
Process >> step: aContext [
	"Resume self until aContext is on top, or if already on top, do next step"

	^Processor activeProcess
		evaluate:
			[self suspendedContext == aContext
				ifTrue: [suspendedContext := suspendedContext step]
				ifFalse: [self complete: (self calleeOf: aContext)]]
		onBehalfOf: self
]

{ #category : '*Debugging-Core' }
Process >> stepToSendOrReturn [

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext stepToSendOrReturn]
		onBehalfOf: self
]
