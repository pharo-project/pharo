operations
next
	self hasNext ifFalse: [ ^ LGit_GIT_ITEROVER signal ].
	
	^ self basicNext