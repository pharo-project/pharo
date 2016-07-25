actions
commit
	[  
		UIManager default 
			informUser: 'Executing ', self title 
			during: [ self unit execute ] ]
	on: VSCommitProjectValidationError 
	do: [ :e | ^ self informValidationError: e ].
	self unit projectVersion 
		ifNotNil: [ :version | version open ].
	self unit comment 
		ifNotNil: [ :comment | 
			self class commentCache add: comment.
			self unit comment: nil ].
	self refresh