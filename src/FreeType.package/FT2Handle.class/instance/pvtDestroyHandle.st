private
pvtDestroyHandle
	"This should only be sent from the finalizer. 
	 It runs inside a mutex because in strange cases it can happen that this is executed twice, 
	 causing a primitiveFailed to be raised."
	self class destroyMutex critical: [ | handleToRelease |
		self isValid ifFalse: [ ^self ].
		handleToRelease := self handle copy.
		self primDestroyHandle.
		"TODO this is bad, but it will prevent duplicated handles to be released"
		FT2Handle allSubInstancesDo: [ :each | 
			(handleToRelease = each handle) ifTrue: [ each beNull ] ] ]