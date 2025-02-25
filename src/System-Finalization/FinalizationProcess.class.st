"
I represent the pharo finalization process that waits for finalization of objects and sends the #mourn message to then. I am registered to the startup to stop/start myself on each start.
"
Class {
	#name : 'FinalizationProcess',
	#superclass : 'Object',
	#classVars : [
		'FinalizationSemaphore',
		'TheFinalizationProcess'
	],
	#category : 'System-Finalization-Base',
	#package : 'System-Finalization',
	#tag : 'Base'
}

{ #category : 'accessing' }
FinalizationProcess class >> finalizationProcess [
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The VM signals FinalizationSemaphore whenever
	 the queue is non-empty.  This process loops, waiting on the semaphore, fetches the first element
	 of the queue and then spawns a process at a higher priority to actually send the mourn messages.
	 If an error occurs in the higher priority mourn loop process then this process will simply spawn
	 another process, hence ensuring that errors in finalization methods don't break finalization.

	 In addition this process also runs the old finalization scheme, supporting clients of the older,
	 WeakRegistry based scheme.  Hopefully this will go away when all clients have moved over."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue: [FinalizationSemaphore wait; initSignals.
	 "Support the old scheme until things have changed over..."
	 [firstMourner := self primitiveFetchMourner.
	  firstMourner isNotNil] whileTrue:
		[[throttle signal.
		  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1.
		 throttle wait]]
]

{ #category : 'class initialization' }
FinalizationProcess class >> initialize [
	"Do we need to initialize specialObjectsArray?"

	Smalltalk specialObjectsArray size < 42 ifTrue: [ Smalltalk recreateSpecialObjectsArray ].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42
]

{ #category : 'private - mourning' }
FinalizationProcess class >> mournLoopWith: firstMourner [
	"Send mourn to all the objects available in the mourn queue, starting
	 with firstMourner which the sender has already extracted for us.  If
	 an error occurs here, it will break this loop but the sender will spawn
	 another mournLoopWith: so that finalization is not broken by errors in
	 individual cases."
	| mourner |
	mourner := firstMourner.
	[
	mourner mourn.
	 (mourner := self primitiveFetchMourner) isNotNil] whileTrue
]

{ #category : 'private - mourning' }
FinalizationProcess class >> primitiveFetchMourner [
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withCRs].
	^nil
]

{ #category : 'mourning' }
FinalizationProcess class >> restartFinalizationProcess [
	<script>
	"kill any old process, just in case"
	TheFinalizationProcess
		ifNotNil: [TheFinalizationProcess terminate.
			TheFinalizationProcess := nil].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	TheFinalizationProcess := [self finalizationProcess]
		forkAt: Processor userInterruptPriority.
	TheFinalizationProcess name: 'Finalization Process'
]

{ #category : 'cleanup' }
FinalizationProcess class >> restartMethods [

	self restartFinalizationProcess
]

{ #category : 'accessing' }
FinalizationProcess class >> runningFinalizationProcess [
	"Answer the FinalizationProcess I am running, if any"
	^TheFinalizationProcess
]

{ #category : 'system startup' }
FinalizationProcess class >> startUp: resuming [
	resuming ifFalse: [ ^self ].
	self restartFinalizationProcess
]
