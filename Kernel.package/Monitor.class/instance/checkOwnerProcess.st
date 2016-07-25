private
checkOwnerProcess
	self isOwnerProcess
		ifFalse: [self error: 'Monitor access violation'].