"
I am a simple strategy to run the callbacks always in the same process.
"
Class {
	#name : #TFCallbackSameProcessRunStrategy,
	#superclass : #TFCallbackForkRunStrategy,
	#instVars : [
		'callbackProcess'
	],
	#classVars : [
		'Instance'
	],
	#category : #'ThreadedFFI-Callbacks'
}

{ #category : #'instance creation' }
TFCallbackSameProcessRunStrategy class >> new [

	self error: 'Use uniqueInstance'
]

{ #category : #'instance creation' }
TFCallbackSameProcessRunStrategy class >> uniqueInstance [ 

	^ Instance ifNil: [ Instance := self basicNew initialize; yourself]
]

{ #category : #private }
TFCallbackSameProcessRunStrategy >> callbackProcess [

	(callbackProcess isNotNil and: [ callbackProcess isTerminated not ])
		ifTrue: [ ^ callbackProcess ].
		
	callbackProcess := [ self waitForever ] newProcess.
	callbackProcess priority: Processor highIOPriority - 1.
	callbackProcess resume.
	^ callbackProcess.
]

{ #category : #private }
TFCallbackSameProcessRunStrategy >> doExecuteCallback: aCallbackInvocation on: aTFRunner [

	aTFRunner handleExceptionDuring: [ aCallbackInvocation execute ]
]

{ #category : #executing }
TFCallbackSameProcessRunStrategy >> executeCallback: aCallbackInvocation on: aTFRunner [

	| process newContext methodToUse |
	process := self callbackProcess.
		
	"I will reuse the same process if it is not used already. Reentrant callbacks should fork a new process"
	(process isSuspended and: [ process suspendedContext compiledCode method = (self class >> #waitForever) ])
		ifFalse: [ ^ super executeCallback: aCallbackInvocation on: aTFRunner ].	

	methodToUse := self class >> #doExecuteCallback:on:.
	
	newContext := Context 
		sender: process suspendedContext 
		receiver: self 
		method: methodToUse
		arguments: {aCallbackInvocation. aTFRunner}.

	process suspendedContext: newContext.
	process resume
]

{ #category : #private }
TFCallbackSameProcessRunStrategy >> waitForever [

	[ true ] whileTrue: [ Processor activeProcess suspend ]
]
