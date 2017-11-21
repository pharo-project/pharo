"
I provide synchronized communication of a single bit of information (a ""signal"") between Processes. A signal is sent by sending the message signal and received by sending the message wait. If no signal has been sent when a wait message is sent, the sending Process will be suspended until a signal is sent.
"
Class {
	#name : #Semaphore,
	#superclass : #LinkedList,
	#instVars : [
		'excessSignals'
	],
	#category : #Kernel-Processes
}

{ #category : #'instance creation' }
Semaphore class >> forMutualExclusion [
	"Answer an instance of me that contains a single signal. This new 
	instance can now be used for mutual exclusion (see the critical: message 
	to Semaphore)."

	^self new signal
]

{ #category : #'instance creation' }
Semaphore class >> new [
	"Answer a new instance of Semaphore that contains no signals."

	^self basicNew initSignals
]

{ #category : #comparing }
Semaphore >> = anObject [
	^ self == anObject
]

{ #category : #initialize-release }
Semaphore >> consumeAllSignals [
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0.
]

{ #category : #'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock [			
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in
	the process of running the critical: message. If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."
	
	"We need to catch eventual interruptions very carefully.
	The naive approach of just doing, e.g.,:
			self wait.
			aBlock ensure: [self signal].
	will fail if the active process gets terminated while in the wait.
	However, the equally naive:
			[self wait.
			aBlock value] ensure: [self signal].
	will fail too, since the active process may get interrupted while
	entering the ensured block and leave the semaphore signaled twice.
	To avoid both problems we make use of the fact that interrupts only
	occur on sends (or backward jumps) and use an assignment (bytecode)
	right before we go into the wait primitive (which is not a real send and
	therefore not interruptable either)."
	
	| blockValue caught |
	caught := false.
	[
		caught := true.
		self wait.
		blockValue := mutuallyExcludedBlock value
	] ensure: [caught ifTrue: [self signal]].
	^blockValue

]

{ #category : #'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifCurtailed: terminationBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	^self critical:[mutuallyExcludedBlock ifCurtailed: terminationBlock]

]

{ #category : #'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifError: errorBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	| blockValue hasError errMsg errRcvr |
	hasError := false.
	self critical:[
		blockValue := mutuallyExcludedBlock ifError: [ :msg :rcvr|
			hasError := true.
			errMsg := msg.
			errRcvr := rcvr
		].
	].
	hasError ifTrue:[ ^errorBlock cull: errMsg cull: errRcvr].
	^blockValue
]

{ #category : #'mutual exclusion' }
Semaphore >> critical: mutuallyExcludedBlock ifLocked: alternativeBlock [
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	
	"Note: The following is tricky and depends on the fact that the VM will not switch between 
processes while executing byte codes (process switches happen only in real sends). The following 
test is written carefully so that it will result in bytecodes only."

	excessSignals == 0 
		ifTrue: 
			[ "If we come here, then the semaphore was locked when the test executed. 
			Evaluate the alternative block and answer its result."
			^alternativeBlock value ].
	^self critical: mutuallyExcludedBlock
]

{ #category : #'process termination handling' }
Semaphore >> handleProcessTerminationOfWaitingContext: suspendedContext [

	^suspendedContext method == (Semaphore compiledMethodAt: #critical:) 
		ifTrue: [ suspendedContext home]
		ifFalse: [ suspendedContext]
]

{ #category : #comparing }
Semaphore >> hash [
	^ self identityHash
]

{ #category : #initialize-release }
Semaphore >> initSignals [
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0.
]

{ #category : #testing }
Semaphore >> isSignaled [
	"Return true if this semaphore is currently signaled"
	^excessSignals > 0
]

{ #category : #initialize-release }
Semaphore >> resumeProcess: aProcess [
	"Remove the given process from the list of waiting processes (if it's there) and resume it.  This is used when a process asked for its wait to be timed out."

	| process |
	process := self remove: aProcess ifAbsent: [nil].
	process ifNotNil: [process resume].
]

{ #category : #communication }
Semaphore >> signal [
	"Primitive. Send a signal through the receiver. If one or more processes 
	have been suspended trying to receive a signal, allow the first one to 
	proceed. If no process is waiting, remember the excess signal. Essential. 
	See Object documentation whatIsAPrimitive."

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"


]

{ #category : #initialize-release }
Semaphore >> terminateProcess [
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].
]

{ #category : #communication }
Semaphore >> wait [
	"Primitive. The active Process must receive a signal through the receiver 
	before proceeding. If no signal has been sent, the active Process will be 
	suspended until one is sent. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals := excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"

]

{ #category : #communication }
Semaphore >> wait: aDuration [
	"Wait on this semaphore for up to the given time duration, then timeout. 
	Return true if the deadline expired, false otherwise."
	| d |
	d := DelayWaitTimeout new setDelay: aDuration asMilliSeconds forSemaphore: self.
	^d wait
]

{ #category : #communication }
Semaphore >> wait: aDuration onCompletion: completionBlock onTimeout: timeoutBlock [
	"Wait on this semaphore for up to the given time duration, then timeout.
	If the deadline expired execute timeoutBlock, otherwise execute completionBlock.
	Return the value returned by the executed block."
	| d |
	d := DelayWaitTimeout new setDelay: aDuration asMilliSeconds forSemaphore: self.
	^d waitOnCompletion: completionBlock onTimeout: timeoutBlock
]

{ #category : #communication }
Semaphore >> waitTimeoutMSecs: anInteger [
	"Wait on this semaphore for up to the given number of milliseconds, then timeout. 
	Return true if the deadline expired, false otherwise."
	| d |
	d := DelayWaitTimeout new setDelay: (anInteger max: 0) forSemaphore: self.
	^d wait
]

{ #category : #communication }
Semaphore >> waitTimeoutSeconds: anInteger [
	"Wait on this semaphore for up to the given number of seconds, then timeout.
	Return true if the deadline expired, false otherwise."
	^self waitTimeoutMSecs: anInteger * 1000.

]

{ #category : #communication }
Semaphore >> waitTimeoutSeconds: anInteger onCompletion: completionBlock onTimeout: timeoutBlock [
	"Wait on this semaphore for up to the given number of seconds, then timeout.
	If the deadline expired execute timeoutBlock, otherwise execute completionBlock.
	Return the value returned by the executed block."
	| d |
	d := DelayWaitTimeout new setDelay: (anInteger * 1000 max: 0) forSemaphore: self.
	^d waitOnCompletion: completionBlock onTimeout: timeoutBlock
]
