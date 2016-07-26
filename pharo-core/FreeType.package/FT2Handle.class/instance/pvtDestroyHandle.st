private
pvtDestroyHandle
	"This should only be sent from the finalizer. 
	 It runs inside a mutex because in strange cases it can happen that this is executed twice, 
	 causing a primitiveFailed to be raised."
	self class destroyMutex critical: [ 
		self isValid ifFalse: [ ^self ].
		self primDestroyHandle.
		self beNull ].