operations
create
	self exists ifTrue: [ ^ self ].
	
	self repositoryReference ensureCreateDirectory