Extension { #name : #WeakArray }

{ #category : #'*System-Finalization' }
WeakArray class >> doOldFinalization [
	"Process the weak registries, in the old finalization style.  Hopefully this will
	 eventually go away when all clients have adopted the new finalization scheme."
	FinalizationLock critical: [
			FinalizationDependents do: 	[:weakDependent |
				weakDependent ifNotNil: [
					[ weakDependent finalizeValues ] on: Exception fork: [:ex | ex pass ] ]]].
]

{ #category : #'*System-Finalization' }
WeakArray class >> finalizationProcess [
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The VM signals FinalizationSemaphore whenever
	 the queue is non-empty.  This process loops, waiting on the semaphore, fetches the first element
	 of the queue and then spawns a process at a higher priority to acually send the mourn messages.
	 If an error occurs in the higher priority mourn loop process then this process will simply spawn
	 another process, hence ensuring that errors in finalization methods don't break finalization.

	 In addition this process also runs the old finalization scheme, supporting clients of the older,
	 WeakRegistry based scheme.  Hopefully this will go away when all cleints have moved over."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue: [FinalizationSemaphore wait; initSignals.
	 "Support the old scheme until things have changed over..."
	 self doOldFinalization.
	 [firstMourner := self primitiveFetchMourner.
	  firstMourner notNil] whileTrue:
		[[throttle signal.
		  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1.
		 throttle wait]]
]

{ #category : #'*System-Finalization' }
WeakArray class >> mournLoopWith: firstMourner [
	"Send mourn to all the objects available in the mourn queue, starting
	 with firstMourner which the sender has already extraced for us.  If
	 an error occurs here, it will break this loop but the sender will spawn
	 another mournLoopWith: so that finalization is not broken by errors in
	 individual cases."
	| mourner |
	mourner := firstMourner.
	[mourner mourn.
	 (mourner := self primitiveFetchMourner) notNil] whileTrue
]

{ #category : #'*System-Finalization' }
WeakArray class >> primitiveFetchMourner [
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withCRs].
	^nil
]

{ #category : #'*System-Finalization' }
WeakArray class >> restartFinalizationProcess [
	"kill any old process, just in case"
	FinalizationProcess
		ifNotNil: [FinalizationProcess terminate.
			FinalizationProcess := nil].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	
	FinalizationProcess := [self finalizationProcess]
		forkAt: Processor userInterruptPriority.
	FinalizationProcess name: 'WeakArray Finalization Process'.
]

{ #category : #'*System-Finalization' }
WeakArray class >> startUp: resuming [
	resuming ifFalse: [ ^self ].
	self restartFinalizationProcess.
]
