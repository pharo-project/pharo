private
primitiveComputeLiterals
	| semaphore |
	literals := IdentitySet new: 25000.
	literals addAll: self specialSelectors keys.
	selectors := IdentitySet new.
	RBBrowserEnvironment new
		classesDo: [ :each | self computeLiteralsForClass: each ].
	semaphore := literalSemaphore.
	literalSemaphore := nil.
	self signalProcesses: semaphore.
	^literalProcess := nil