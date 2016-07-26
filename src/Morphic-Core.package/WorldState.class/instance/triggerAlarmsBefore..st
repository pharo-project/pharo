alarms
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."
	| pending |
	lastAlarmTime ifNil:[lastAlarmTime := nowTime].
	(nowTime < lastAlarmTime or:[nowTime - lastAlarmTime > 10000])
		ifTrue:[self adjustAlarmTimes: nowTime].
	pending := self alarms.
	[pending isEmpty not and:[pending first scheduledTime < nowTime]]
		whileTrue:[pending removeFirst value: nowTime].
	lastAlarmTime := nowTime.