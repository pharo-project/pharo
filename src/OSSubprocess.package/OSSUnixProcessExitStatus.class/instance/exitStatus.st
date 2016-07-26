accessing
exitStatus
	self isExited ifTrue: [^self statusIfExited].
	self isSignaled ifTrue: [^self statusIfSignaled].
	self isStopped ifTrue: [^self statusIfStopped].
	self notify: 'cannot decode exit status ', intValue asString.
	^intValue