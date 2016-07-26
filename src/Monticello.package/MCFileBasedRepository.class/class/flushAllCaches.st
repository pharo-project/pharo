actions
flushAllCaches
	self allSubInstancesDo: [:ea | ea flushCache]