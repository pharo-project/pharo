initialization
initialize
	"Do we need to initialize specialObjectsArray?"
	Smalltalk specialObjectsArray size < 42 
		ifTrue: [Smalltalk recreateSpecialObjectsArray].

	SessionManager default
		registerSystemClassNamed: self name.

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].