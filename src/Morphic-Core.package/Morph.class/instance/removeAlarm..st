events-alarms
removeAlarm: aSelector
	"Remove the given alarm"
	| scheduler |
	scheduler := self alarmScheduler.
	scheduler ifNotNil:[scheduler removeAlarm: aSelector for: self].