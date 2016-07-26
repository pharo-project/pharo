events-alarms
addAlarm: aSelector with: arg1 after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1) after: delayTime