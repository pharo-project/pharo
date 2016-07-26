event handling
removeLink: actionCode
	self eventHandler ifNotNil:
		[self eventHandler on: actionCode send: nil to: nil]