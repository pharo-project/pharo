public accessing
beReadOnly
	self readOnly
		ifFalse: [ self readOnly: true ]