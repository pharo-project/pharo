updating
"protocol: updating"
finalizePreviousSession
	"This method is likely called at image startup and it's job is to finalize
	stuff related to the previous run and let everything clean so that 
	the unique instance vmProcess of OSSVMProcess can be initialized correctly at startup. "
	childWatcher ifNotNil: [ 
		childWatcher isTerminated ifFalse: [ childWatcher terminate ].
		childWatcher := nil.
	].
	sigChldSemaphore ifNotNil: [
		self systemAccessor restoreSigChld.
		sigChldSemaphore := nil
	]
