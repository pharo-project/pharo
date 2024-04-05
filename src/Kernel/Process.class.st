"
I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced partly using the value of priority.

(If anyone ever makes a subclass of Process, be sure to use allSubInstances in anyProcessesAbove:.)


Process-specific storage: 

	An old implementation using #environmentAt: [ifAbsent:/put:] protocol are no longer supported.
	One must not use a process-specific storage (PSS) methods directly, and instead use ProcessSpecificVariable (or subclass) instances to access process-specific storage.
	
A new implemention is a revision towards making an access to PSS faster.

When new instance of ProcessSpecificVariable are created, it obtains an unique index, which is registered using #allocatePSKey: (see class side).
This allows to dynamically create as many process-specific variables as needed, and access them in fast manner via simple array index (instead of dictionary lookup,
as in previous implementation).

Another important aspect of new implementation is that all values in PSS are held weakly. This is done to prevent accidental memory leaks
as well as no need to manually unregistering a process-specific keys , once they are no longer in use.
"
Class {
	#name : 'Process',
	#superclass : 'Link',
	#instVars : [
		'suspendedContext',
		'priority',
		'myList',
		'name',
		'env',
		'effectiveProcess',
		'terminating',
		'level',
		'errorHandler'
	],
	#classVars : [
		'InheritablePSKeys',
		'PSKeys',
		'PSKeysSema'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'process specific' }
Process class >> allocatePSKey: aPSVariable [

	"Add a new process-specific key.
	If an object already registered as a key, answer its index,
	if object is not registered, first search for an empty slot for insertion and if not found, grow an array to add new object"

	| index |
	self psKeysSema critical: [
		PSKeys
			ifNil: [ PSKeys := WeakArray with: aPSVariable. index := 1 ]
			ifNotNil: [
				index := PSKeys indexOf: aPSVariable.
				index = 0 ifTrue: [
					index := PSKeys indexOf: nil.
					index = 0
						ifTrue: [
							index := (PSKeys := PSKeys copyWith: aPSVariable) size ]
						ifFalse: [
							"Yes, this is slow, but we have to make sure that if we reusing index,
							all existing processes having value at given index reset to nil.
							We don't care if new processes will be created during this loop,
							since their env variable will be initially nil anyways, hence nothing to reset "
							Process allSubInstancesDo: [:p | p resetPSValueAt: index ].
							PSKeys at: index put: aPSVariable.
							]
				]
			].

		aPSVariable isInheritable ifTrue: [
			InheritablePSKeys
				ifNil: [ InheritablePSKeys := Array with: index ]
				ifNotNil: [
					(InheritablePSKeys includes: index) ifFalse: [ InheritablePSKeys := InheritablePSKeys copyWith: index ]]
		]
	].

	^ index
]

{ #category : 'instance creation' }
Process class >> forContext: aContext priority: anInteger [
	"Answer an instance of me that has suspended aContext at priority
	anInteger."

	| newProcess |
	newProcess := self new.
	newProcess suspendedContext: aContext asContext.
	newProcess priority: anInteger.
	Processor activeProcess installEnvIntoForked: newProcess.
	^newProcess
]

{ #category : 'process specific' }
Process class >> psKeysSema [
	"Isolate handling of class variable"

	^PSKeysSema ifNil: [ PSKeysSema := Semaphore forMutualExclusion ]
]

{ #category : 'process specific' }
Process class >> updateInheritableKeys [
"
	self updateInheritableKeys
"
	| keys |
	keys := Set new.
	ProcessSpecificVariable allSubclasses select: [ :each | each isInheritable ] thenDo: [ :each | keys add: each soleInstance index].

	InheritablePSKeys := keys asArray ifEmpty: [ nil ]
]

{ #category : 'printing' }
Process >> browserPrintString [
	^self browserPrintStringWith: suspendedContext
]

{ #category : 'printing' }
Process >> browserPrintStringWith: anObject [
	| stream |
	stream := (String new: 100) writeStream.
	stream nextPut: $(.
	priority printOn: stream.
	self isSuspended
		ifTrue: [stream nextPut: $s].
	stream nextPutAll: ') '.
	stream nextPutAll: self name.
	stream nextPut: $:.
	stream space.
	stream nextPutAll: anObject asString.
	^ stream contents
]

{ #category : 'accessing' }
Process >> calleeOf: aContext [
	"Return the context whose sender is aContext.  Return nil if aContext is on top.  Raise error if aContext is not in process chain."

	suspendedContext == aContext ifTrue: [^ nil].
	^ (suspendedContext findContextSuchThat: [:c | c sender == aContext])
		ifNil: [self error: 'aContext not in process chain']
]

{ #category : 'accessing' }
Process >> copyStack [

	^ self copy install: suspendedContext copyStack
]

{ #category : 'private' }
Process >> doTerminationFromAnotherProcess [ 
	"Stop this process forever from another process.
	Unwind to execute pending ensure:/ifCurtailed: blocks before terminating.
	It assumes that self is not the active process and the termination is requested from another process.
	It assumes the terminator process won't resume before the process represented by self is terminated."	

	[ ] ensure: [ | oldList context terminator |
		terminating := true.
		oldList := self suspend.
		suspendedContext ifNil: [^self].
		context := oldList handleProcessTerminationOfWaitingContext: suspendedContext.
		terminator := Semaphore new.
		context bottomContext insertSender: 
			(Context contextOn: UnhandledException do: [:ex | terminator signal. ex pass]).
		context bottomContext insertSender: (Context contextEnsure: [terminator signal]).
		suspendedContext := context unwindAndStop: self.
		self priority: Processor activePriority; resume.
		terminator wait
	]
]

{ #category : 'private' }
Process >> doTerminationFromYourself [
	"Stop this process forever from the process itself.
	Unwind to execute pending ensure:/ifCurtailed: blocks before terminating.
	It assumes that self is the active process. "
	
	| context |
	terminating := true.
	context := thisContext.
	^[[] ensure: [context unwindTo: nil]. self endProcess] asContext jump
]

{ #category : 'accessing' }
Process >> effectiveProcess [
	"effectiveProcess is a mechanism to allow process-faithful debugging.  The debugger executes code
	 on behalf of processes, so unless some effort is made the identity of Processor activeProcess is not
	 correctly maintained when debugging code.  The debugger uses evaluate:onBehalfOf: to assign the
	 debugged process as the effectiveProcess of the process executing the code, preserving process
	 identity."
	^effectiveProcess ifNil: [self]
]

{ #category : 'accessing' }
Process >> effectiveProcess: aProcess [
	effectiveProcess := aProcess
]

{ #category : 'changing process state' }
Process >> endProcess [
	"When I reach this method, I'm terminated. Suspending or terminating me is harmless."

	thisContext terminateTo: nil.   "set thisContext sender to nil"
	self suspend.

	"Restart this method.
	This will terminate and suspend again in case of sending resume to a terminated process"
	"the following line commented out because current debugger step over tests fail with this feature"
	"^thisContext restart"
]

{ #category : 'error handling' }
Process >> errorHandler [

	^ errorHandler ifNil: [ ErrorHandler defaultErrorHandler ]
]

{ #category : 'accessing' }
Process >> errorHandler: anObject [

	errorHandler := anObject
]

{ #category : 'private' }
Process >> evaluate: aBlock onBehalfOf: aProcess [
	"Evaluate aBlock setting effectiveProcess to aProcess.  Used
	 in the execution simulation machinery to ensure that
	 Processor activeProcess evaluates correctly when debugging."
	| oldEffectiveProcess |
	oldEffectiveProcess := effectiveProcess.
	effectiveProcess := aProcess.
	^aBlock ensure: [effectiveProcess := oldEffectiveProcess]
]

{ #category : 'error handling' }
Process >> handleError: anError [

	^ self errorHandler handleError: anError
]

{ #category : 'error handling' }
Process >> handleNotification: aNotification [

	^ self errorHandler handleNotification: aNotification
]

{ #category : 'error handling' }
Process >> handleWarning: aWarning [

	^ self errorHandler handleWarning: aWarning
]

{ #category : 'initialization' }
Process >> initialize [
	super initialize.

	terminating := false
]

{ #category : 'changing suspended state' }
Process >> install: aContext [
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext := aContext
]

{ #category : 'process specific' }
Process >> installEnvIntoForked: newProcess [
	env ifNil: [ ^ self ].
	InheritablePSKeys ifNil: [ ^self ].

	"InheritablePSKeys includes indices of all inheritable variables"
	1 to: InheritablePSKeys size do: [ :i | | varIndex varValue |
		varIndex := InheritablePSKeys at: i.
		(varIndex <= env size) "if new variable was installed into system existed processes env can not have room for it"
			ifTrue: [
				varValue := env at: varIndex.
				varValue ifNotNil: [ (PSKeys at: varIndex) installValue: varValue intoForked: newProcess from: self ] ]]
]

{ #category : 'testing' }
Process >> isActive [
	"A process is #active if it is currently executing.
	There can be only one such process, held in a slot on the Processor."

	^ self state == #active
]

{ #category : 'testing' }
Process >> isActiveProcess [
	"A process is #active if it is currently executing.
	There can be only one such process, held in a slot on the Processor."

	self
		deprecated: 'Please use #isActive instead'
		transformWith: '`@receiver isActiveProcess' -> '`@receiver isActive'.
	^ self isActive
]

{ #category : 'testing' }
Process >> isReady [
	"A #ready process is one that is waiting on one of the priority lists
	on the Processor, and will run as soon as there are no others ahead of it
	or at a higher priority (i.e. the Processor is free)."

	^ self state == #ready
]

{ #category : 'testing' }
Process >> isSuspended [
	"A #suspended process will not run until sent #resume. New processes
	start in this state (though the #fork family sends #resume internally),
	and can be put back by sending #suspend. Effectively this is the 'default'
	state, recognized by the absence of any of the others--not active, not
	terminated, no list on which to be #waiting or #ready."

	^ self state == #suspended
]

{ #category : 'testing' }
Process >> isTerminated [
	"A process is #terminated if one of the following conditions is met:
	(1) the receiver is a defunct process (suspendedContext = nil or pc = nil)
	(2) the receiver is suspended in the endProcess method (we cannot use pragmas...
	pragmas require the compiler, it generates an evil dependency.
	It also is not available in minimal images)"

	^ self state == #terminated
]

{ #category : 'testing' }
Process >> isTerminating [
	"lazy initialization is a fallback only for processes that existed before this addition"
	^ terminating ifNil: [ false ]
]

{ #category : 'testing' }
Process >> isWaiting [
	"A #waiting process is one that is waiting for a Semaphore to be signaled
	(having previously executed a #wait on that Semaphore, thus the name).
	This differs from a #ready process, which also is 'waiting' in a list,
	in that the Semaphore must be explicitly signaled rather than just
	the Processor being free."

	^ self state == #waiting
]

{ #category : 'accessing' }
Process >> name [

 	^name ifNil: [ self hash asString forceTo: 10 paddingStartWith: $ ]
]

{ #category : 'accessing' }
Process >> name: aString [

	name := aString
]

{ #category : 'signaling' }
Process >> on: exception do: handlerAction [
	"This method inject new bottom context into process with exception handler.
	It uses context jump tricks to achieve it"
	| currentContext root newRoot |
	currentContext := self isActive ifTrue: [ thisContext ] ifFalse: [self suspendedContext].
	root := currentContext bottomContext.
	newRoot := [
			[root insertSender: thisContext.
			currentContext jump] on: exception do: handlerAction.
		Processor terminateRealActive] asContext.

	self isActive
		ifTrue: [ newRoot jump ]
		ifFalse: [ self install: newRoot ]
]

{ #category : 'changing process state' }
Process >> primitiveResume [
	"Primitive. Allow the process that the receiver represents to continue. Put
	the receiver in line to become the activeProcess. Fail if the receiver is
	already waiting in a queue (in a Semaphore or ProcessScheduler).
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 87>
	self primitiveFailed
]

{ #category : 'printing' }
Process >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' in '.
	suspendedContext printOn: aStream
]

{ #category : 'accessing' }
Process >> priority [
	"Answer the priority of the receiver."

	^priority
]

{ #category : 'accessing' }
Process >> priority: anInteger [
	"Set the receiver's priority to anInteger.
	When changing the process priority we need to yield to make it reschedule the processor"

	(anInteger between: Processor lowestPriority and: Processor highestPriority)
		ifTrue: [ priority := anInteger. Processor interpriorityYield: self ]
		ifFalse: [ self error: 'Invalid priority: ' , anInteger printString ]
]

{ #category : 'process specific' }
Process >> psValueAt: index [
	"Answer a process-specific value at given index, or nil if value at given index is not defined"

	"NOTE: this method are PRIVATE. Do not use it directly, instead use ProcessSpecificVariable (or its subclasses) "
	env ifNil: [ ^ nil ].
	^ env at: index ifAbsent: nil
]

{ #category : 'process specific' }
Process >> psValueAt: index put: value [
	"Set a value for given index in process-specific storage"

	"NOTE: this method are PRIVATE. Do not use it directly, instead use ProcessSpecificVariable (or its subclasses) "

	env ifNil: [ env := WeakArray new: PSKeys size ].
	env size < PSKeys size ifTrue: [ env := env grownBy: PSKeys size - env size ].
	^ env at: index put: value
]

{ #category : 'signaling' }
Process >> pvtSignal: anException list: aList [
	"Private. This method is used to signal an exception from another
	process...the receiver must be the active process.  If the receiver
	was previously waiting on a Semaphore, then return the process
	to the waiting state after signaling the exception and if the Semaphore
	has not been signaled in the interim"

	"Since this method is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because I believe that could
	have the potential to push an unwanted object on the caller's stack)."
	<debuggerCompleteToSender>
	| blocker |
	self isActive ifFalse: [^self].
	anException signal.
	blocker := Semaphore new.
	[self suspend.
	suspendedContext := suspendedContext swapSender: nil.
	aList class == Semaphore
		ifTrue:
			[aList isSignaled
				ifTrue:
					[aList wait.  "Consume the signal that would have restarted the receiver"
					self resume]
				ifFalse:
					["Add us back to the Semaphore's list (and remain blocked)"
					myList := aList.
					aList add: self]]
		ifFalse: [self resume]] fork.
	blocker wait
]

{ #category : 'process specific' }
Process >> resetPSValueAt: index [

	"NOTE: this method are PRIVATE. "

	env ifNil: [ ^ self ].
	index > env size ifTrue: [ ^ self ].

	env at: index put: nil
]

{ #category : 'changing process state' }
Process >> resume [
	"Allow the process that the receiver represents to continue. Put
	the receiver in line to become the activeProcess. Check for a nil
	suspendedContext, which indicates a previously terminated Process that
	would cause a vm crash if the resume attempt were permitted"

	suspendedContext ifNil: [^ self primitiveFailed].
	^ self primitiveResume
]

{ #category : 'changing process state' }
Process >> run [
	"Suspend current process and execute self instead"

	| proc |
	proc := Processor activeProcess.
	[	proc suspend.
		self resume.
	] forkAt: Processor highestPriority
]

{ #category : 'signaling' }
Process >> signalException: anException [
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"
	| oldList |
	"If we are the active process, go ahead and signal the exception"
	self isActive ifTrue: [^anException signal].

	"Suspend myself first to ensure that I won't run away in the
	midst of the following modifications."
	myList ifNotNil:[oldList := self suspend].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := Context
		sender: suspendedContext
		receiver: self
		method: (self class lookupSelector: #pvtSignal:list:)
		arguments: (Array with: anException with: oldList).

	"If we are on a list to run, then suspend and restart the receiver
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	oldList ifNotNil: [self resume]
]

{ #category : 'testing' }
Process >> state [
	"A process is #active if it is currently executing.
	There can be only one such process, held in a slot on the Processor."
	self == Processor activeProcess ifTrue: [ ^ #active ].
	"A process is #terminated if one of the following conditions is met:
	(1) the receiver is a defunct process (suspendedContext = nil or pc = nil)
	(2) the receiver is suspended in the endProcess method (we cannot use pragmas...
	pragmas require the compiler, it generates an evil dependency.
	It also is not available in minimal images)"
	(suspendedContext isNil or: [
		 suspendedContext isDead or: [
			 suspendedContext method == (self class >> #endProcess) ] ])
		ifTrue: [ ^ #terminated ].
	"Any other process without a list is considered #suspended.
	This is either a new process that has never been sent #resume,
	or one that has been sent #suspend."
	myList ifNil: [ ^ #suspended ].
	"We term a process that is waiting in a list on the Processor #ready,
	because it will run as soon as the Processor is free. Any other type of list,
	usually a Semaphore, is considered #waiting, as the usual way to reach this state
	is by executing e.g. Semaphore>>wait."
	^ myList class == ProcessList
		  ifTrue: [ #ready ]
		  ifFalse: [ #waiting ]
]

{ #category : 'changing suspended state' }
Process >> step [

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext step]
		onBehalfOf: self
]

{ #category : 'changing suspended state' }
Process >> stepToCallee [
	"Step until top context changes"

	Processor activeProcess
		evaluate:
			[| ctxt |
			ctxt := suspendedContext.
			[ctxt == suspendedContext] whileTrue: [
				suspendedContext := suspendedContext step]]
		onBehalfOf: self.
	^suspendedContext
]

{ #category : 'changing process state' }
Process >> suspend [
	"Primitive. Stop the process that the receiver represents in such a way
	that it can be restarted at a later time (by sending the receiver the
	message resume). If the receiver represents the activeProcess, suspend it.
	Otherwise remove the receiver from the list of waiting processes.
	The return value of this method is the list the receiver was previously on (if any)."
	| oldList |
	<primitive: 88>
	"This is fallback code for VMs which only support the old primitiveSuspend which
	would not accept processes that are waiting to be run."
	myList ifNil:[^nil]. "this allows us to use suspend multiple times"
	oldList := myList.
	myList := nil.
	oldList remove: self ifAbsent:[].
	^oldList
]

{ #category : 'accessing' }
Process >> suspendedContext [
	"Answer the context the receiver has suspended."

	^suspendedContext
]

{ #category : 'private' }
Process >> suspendedContext: aContext [

	suspendedContext := aContext
]

{ #category : 'accessing' }
Process >> suspendingList [
	"Answer the list on which the receiver has been suspended."

	^myList
]

{ #category : 'changing process state' }
Process >> terminate [
	"Stop the process that the receiver represents forever. Unwind to execute pending ensure: and
	ifCurtailed: blocks before terminating; allow all unwind blocks to run; if they are currently in
	progress, let them finish. If the process is in the middle of a #critical: section, release it properly."

	"If terminating the active process, create a parallel stack and run unwinds from there;
	if terminating a suspended process, create a parallel stack for the process being terminated
	and resume the suspended process to complete its termination from the parallel stack. Use
	a priority higher than the active priority to make the process that invoked the termination
	wait for its completion."

	"If terminating a suspended process (including runnable or blocked), always suspend the 
	terminating process first so it doesn't accidentally get woken up. Equally important is the
	side effect of the suspension. In 2022 a new suspend semantics has been introduced:
	the revised #suspend backs up a process waiting on a conditional variable to the send that
	invoked the wait state, while the pre-2022 #suspend simply removed the process from
	the conditional variable's list it was previously waiting on; figure out if we are terminating
	the process while waiting in Semaphore>>critical:. In this case, pop the suspendedContext
	so that we leave the ensure: block inside Semaphore>>critical: without signaling the semaphore.
	Execute termination wrapped in #ensure to ensure it completes even if the terminator
	process itself gets terminated before it's finished; see testTerminateInTerminate."

	self isActive
		ifTrue: [ self doTerminationFromYourself ]
		ifFalse: [ self doTerminationFromAnotherProcess ]
]
