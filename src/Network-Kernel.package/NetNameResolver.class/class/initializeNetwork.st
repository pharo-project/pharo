network initialization
initializeNetwork
	"Initialize the network drivers and record the semaphore to be used by the resolver. Do nothing if the network is already initialized.."
	"NetNameResolver initializeNetwork"

	| successful  sema|
	 "network is already initialized"
	(self resolverStatus = ResolverUninitialized)
		ifFalse: [^true].
	"No real message sends allowed in the atomic check, so pre-create a semaphore"
	sema := Semaphore forMutualExclusion.
	"Atomically check if another process is in the progress of initializing network.
	If so, block untill it is done and retry, otherwise start setting it up.
	Not doing so could lead to
	- External semaphore leakage (if we both try to set up simultaneously)
	- Returning an incorrect result (if we return a value independent of whether the other process was successful)"
	HaveNetwork == nil ifTrue: [HaveNetwork := sema].
	"Then, enter critical section where other process has initialized, or we need to do it."
	HaveNetwork critical: [ |semaIndex| 
		"If other process initialized while we were blocked, retry to see if it were successful"
		HaveNetwork ifNil: [^self initializeNetwork].
		"If the network has previously been initialized, but now unavailable, we need to unregister semaphore"
		ResolverSemaphore ifNotNil: [Smalltalk unregisterExternalObject: ResolverSemaphore].
		ResolverSemaphore := Semaphore new.
		semaIndex := Smalltalk registerExternalObject: ResolverSemaphore.
		successful := (self primInitializeNetwork: semaIndex) notNil.
		HaveNetwork := nil.].	
	^successful or: [NoNetworkError signal: 'failed network initialization']
	
