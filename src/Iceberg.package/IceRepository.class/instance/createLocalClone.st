repository handling
createLocalClone
	self location parent ensureCreateDirectory.
	IceGitCommand in: self location parent execute: 
		{'clone'.
		branch notNil ifTrue: {'-b'. branch}.
		"(readonly ifNil: false) ifTrue: {'--depth'. '1'}."
		origin url.
		location basename}.
