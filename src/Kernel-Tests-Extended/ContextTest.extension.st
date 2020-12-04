Extension { #name : #ContextTest }

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testCannotReturn [
	| context p |
	p := [context := thisContext] fork.
	[p isTerminated] whileFalse: [ 10 milliSeconds wait ].

	[ context resume: #result.
	self assert: false description: 'ContextCannotReturn should be signaled' ]
		 on: ContextCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err target equals: context]
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testClosureRestart [
	"Test that various combinations of closures are restarted with the expected values"
	"no args, no remote temps blocks are not tested, as I don't know how to do that programatically without ending up looping endlessly"
	self should: [self privRestartBlockTest] notTakeMoreThan: 0.1 second.
	self should: [self privRestartArgBlockTest] notTakeMoreThan: 0.1 second.
	"self should: [self privRestartBlockArgsNoRemoteTempsTest] notTakeMoreThan: 0.1 second" "FAILING!"
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testJump [
	#(exampleClosure exampleSend exampleStore) do: [ :selector |
		self verifyJumpWithSelector: selector ]
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testReadVariableNamed [
	|localVar|
	localVar := 2.
	instVarForTestLookupSymbol := 3.
	classVarForTestLookupSymbol := 4.
	self assert: (thisContext readVariableNamed: #localVar) equals: 2.
	self assert: (thisContext readVariableNamed: #instVarForTestLookupSymbol) equals: 3.
	self assert: (thisContext readVariableNamed: #classVarForTestLookupSymbol) equals: 4.
	self assert: (thisContext readVariableNamed: #Smalltalk) equals: Smalltalk.
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testSourceNodeExecuted [
	| sourceNode |
	sourceNode := thisContext sender sender sourceNodeExecuted.
	self assert: sourceNode selector equals: #performTest
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testSourceNodeExecutedWhenContextIsJustAtStartpc [
	| sourceNode context |
	
	context := Context sender: thisContext receiver: self method: self class >> testSelector arguments: #().
	sourceNode := context sourceNodeExecuted.
	self assert: sourceNode equals: (self class >> testSelector) ast sendNodes first receiver
]

{ #category : #'*Kernel-Tests-Extended' }
ContextTest >> testWriteVariableNamedValue [
	|localVar|
	localVar := 2.
	instVarForTestLookupSymbol := 3.
	classVarForTestLookupSymbol := 4.
	thisContext writeVariableNamed: #localVar value: #changed1.
	self assert: (thisContext readVariableNamed: #localVar) equals: #changed1.
	thisContext writeVariableNamed: #instVarForTestLookupSymbol value: #changed2.
	self assert: (thisContext readVariableNamed: #instVarForTestLookupSymbol) equals: #changed2.
	thisContext writeVariableNamed: #classVarForTestLookupSymbol value: #changed3.
	self assert: (thisContext readVariableNamed: #classVarForTestLookupSymbol) equals: #changed3
]
