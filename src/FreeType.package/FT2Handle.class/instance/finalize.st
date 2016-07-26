finalization
finalize
	"If session has changed, and startup is not yet done, we do not attempt 
	to free external resources"
	Session == Smalltalk session ifFalse: [ ^ self ].
	self pvtDestroyHandle.
