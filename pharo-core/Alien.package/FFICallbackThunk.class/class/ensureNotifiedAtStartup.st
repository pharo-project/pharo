class initialization
ensureNotifiedAtStartup
	"FFICallbackThunk needs to be notified at system startup to clear the ExecutablePages table."
	self ~~ FFICallbackThunk ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: Alien]