protocol
close

	self canBeClosed ifFalse: [ ^ self ].
	self delete.