initialize
aboutToBeRemoved
	self beNotVisible.
	self removeAlarm.
	self textArea announcer unsubscribe: self