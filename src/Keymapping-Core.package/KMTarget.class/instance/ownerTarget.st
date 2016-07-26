accessing
ownerTarget
	morph owner ifNil: [ ^nil ].
	^morph owner kmDispatcher target.