class initialization
ensureNotifiedAtStartup
	"Callback needs to be notified at system startup to clear the ThunkToCallbackMap and determine concreteClass."
	self ~~ Callback ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: FFICallbackThunk]