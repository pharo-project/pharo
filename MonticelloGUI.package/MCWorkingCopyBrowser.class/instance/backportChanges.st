actions
backportChanges
	self canBackport ifFalse: [^self].
	workingCopy ifNotNil:
		[workingCopy needsSaving ifTrue: [^ self inform: 'You must save the working copy before backporting.'].
		self pickAncestorVersionInfo ifNotNil:
			[:baseVersionInfo |
			workingCopy backportChangesTo: baseVersionInfo]]