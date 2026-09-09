Class {
	#name : 'ContextDebuggingTest',
	#superclass : 'TestCase',
	#instVars : [
		'instVar',
		'aSender',
		'aMethodContext'
	],
	#category : 'Debugging-Utils-Tests',
	#package : 'Debugging-Utils-Tests'
}

{ #category : 'helpers' }
ContextDebuggingTest >> accessorMethod [
	^instVar
]

{ #category : 'helpers' }
ContextDebuggingTest >> contextForMethod: aCompiledMethod [

	^ Context
		  sender: thisContext
		  receiver: self class basicNew
		  method: aCompiledMethod
		  arguments: #(  )
]

{ #category : 'helpers' }
ContextDebuggingTest >> deadContext [
	| process context |
	process := [  ] newProcess.
	context := process suspendedContext.
	context pc: nil.
	^ context
]

{ #category : 'private' }
ContextDebuggingTest >> privRestartArgBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	| firstTimeThrough |
	firstTimeThrough := true.

	self
		assert: 30
		equals:
			([ :a |
			| b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			firstTimeThrough
				ifTrue: [ firstTimeThrough := false.
					thisContext restart ].
			b ] value: 10)
]

{ #category : 'private' }
ContextDebuggingTest >> privRestartBlockArgsNoRemoteTempsTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	self
		assert: 30
		equals:
			([ :a :first |
			| b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			first
				ifTrue: [ "Cheat and modify one of the args so we will not loop endlessly"
					thisContext tempAt: 2 put: false.
					thisContext restart ].
			b ] value: 10 value: true)
]

{ #category : 'private' }
ContextDebuggingTest >> privRestartBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	| a firstTimeThrough |
	firstTimeThrough := true.
	a := 10.

	self
		assert: 30
		equals:
			[ | b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			firstTimeThrough
				ifTrue: [ firstTimeThrough := false.
					thisContext restart ].
			b ] value
]

{ #category : 'helpers' }
ContextDebuggingTest >> selfReturnMethod [
	#foo asSymbol
]

{ #category : 'running' }
ContextDebuggingTest >> setUp [

	| aCompiledMethod aReceiver anArgument |
	super setUp.
	aCompiledMethod := Rectangle compiledMethodAt: #areasOutside:.
	aReceiver := 100 @ 100 corner: 200 @ 200.
	aSender := thisContext.
	anArgument := 420 @ 420 corner: 200 @ 200.
	aMethodContext := Context
		                  sender: aSender
		                  receiver: aReceiver
		                  method: aCompiledMethod
		                  arguments: { anArgument }
]

{ #category : 'tests' }
ContextDebuggingTest >> testActivateReturnValue [
	self assert: ((aSender activateReturn: aMethodContext value: #()) isKindOf: Context).
	self assert: (aSender activateReturn: aMethodContext value: #()) receiver equals: aMethodContext
]

{ #category : 'tests' }
ContextDebuggingTest >> testClosureRestart [
	"Test that various combinations of closures are restarted with the expected values"

	"no args, no remote temps blocks are not tested, as I don't know how to do that programatically without ending up looping endlessly"

	self privRestartBlockTest.
	self privRestartArgBlockTest.
	self privRestartBlockArgsNoRemoteTempsTest
]

{ #category : 'tests' }
ContextDebuggingTest >> testUpdatePCForQuickPrimitiveRestart [

	|method newContext  |
	method := self class >> #accessorMethod.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method initialPC + newContext quickMethodPrimitiveBytecodeSize.
		
	method := self class >> #zeroReturnMethod.
	newContext := self contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method initialPC + newContext quickMethodPrimitiveBytecodeSize.
		
	method := self class >> #trueReturnMethod.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method endPC.
		
	method := Symbol >> #asSymbol.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method endPC
]

{ #category : 'helpers' }
ContextDebuggingTest >> trueReturnMethod [
	^true
]

{ #category : 'helpers' }
ContextDebuggingTest >> zeroReturnMethod [
	^0
]
