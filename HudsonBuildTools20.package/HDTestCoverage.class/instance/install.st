actions
install
	reference actualClass methodDict
		at: reference selector
		put: self.
	self flushCache