actions
insertSelected
	context model isEmpty ifTrue: [^ false].
	self insertCompletion: (context model completionAt: self selected).
	self delete.
	^ true