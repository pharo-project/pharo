blinking
ensureFor: millisecs
	self removeAlarm.
	self beVisible.
	self textArea canChangeText
		ifTrue: [ self addAlarmAt: millisecs ]