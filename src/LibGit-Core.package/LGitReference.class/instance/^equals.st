comparing
= anObject
	self class == anObject class ifFalse: [ ^ false ].
	
	^ self == anObject or: [
		(self 
			reference_cmp: self 
			ref2: anObject) = LGitReturnCodeEnum git_ok  ]
	