private
unplug
	self removeAlarm.
	self textArea announcer unsubscribe: self.
	super unplug