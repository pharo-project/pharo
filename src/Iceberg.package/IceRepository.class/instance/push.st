actions
push
	self command: ('push origin {1}' format: {self branch}).
	self refresh.
	self announcer announce: IcePushed new