events-alarms
addAlarm: aSelector withArguments: args at: scheduledTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	| scheduler |
	scheduler := self alarmScheduler.
	scheduler ifNotNil:[scheduler addAlarm: aSelector withArguments: args for: self at: scheduledTime].