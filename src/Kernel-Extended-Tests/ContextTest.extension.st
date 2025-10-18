Extension { #name : 'ContextTest' }

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testBlockCannotReturn [

	| p |
	p := [ thisContext pc: nil ] newProcess.
	[
	p suspendedContext method selector = #pc: and: [
		p suspendedContext sender isDead ] ] whileFalse: [ p step ].
	self assert: p suspendedContext method selector equals: #pc:.
	self assert: p suspendedContext sender isDead.
	self assert: p suspendedContext pc equals: p suspendedContext method endPC.

	p step. "step into a return to a dead sender"

	self assert: p suspendedContext method selector equals: #cannotReturn:
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testCannotReturn [
	| context p |
	p := [context := thisContext] fork.
	[p isTerminated] whileFalse: [ (Delay forMilliseconds: 10) wait ].

	[ context resume: #result.
	self assert: false description: 'ContextCannotReturn should be signaled' ]
		 on: ContextCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err target equals: context]
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testClosureRestart [
	"Test that various combinations of closures are restarted with the expected values"

	"no args, no remote temps blocks are not tested, as I don't know how to do that programatically without ending up looping endlessly"

	self privRestartBlockTest.
	self privRestartArgBlockTest.
	self privRestartBlockArgsNoRemoteTempsTest
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testJump [
	#(exampleClosure exampleSend exampleStore) do: [ :selector |
		self verifyJumpWithSelector: selector ]
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testReadVariableNamed [
	|localVar|
	localVar := 2.
	instVarForTestLookupSymbol := 3.
	classVarForTestLookupSymbol := 4.
	self assert: (thisContext readVariableNamed: #localVar) equals: 2.
	self assert: (thisContext readVariableNamed: #instVarForTestLookupSymbol) equals: 3.
	self assert: (thisContext readVariableNamed: #classVarForTestLookupSymbol) equals: 4.
	self assert: (thisContext readVariableNamed: #Smalltalk) equals: Smalltalk
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testSourceNodeExecuted [
	| sourceNode |
	sourceNode := thisContext sender sender sourceNodeExecuted.
	self assert: sourceNode selector equals: #performTest
]

{ #category : '*Kernel-Extended-Tests' }
ContextTest >> testSourceNodeExecutedWhenContextIsJustAtStartpc [
	| sourceNode context |

	context := Context sender: thisContext receiver: self method: self class >> testSelector arguments: #().
	sourceNode := context sourceNodeExecuted.
	self assert: sourceNode equals: (self class >> testSelector) ast sendNodes first receiver
]
