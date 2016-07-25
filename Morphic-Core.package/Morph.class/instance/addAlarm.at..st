events-alarms
addAlarm: aSelector at: scheduledTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: #() at: scheduledTime