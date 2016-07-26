alarms
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as roll-over, image-startup etc)"
	| deltaTime |
	deltaTime := nowTime - lastAlarmTime.
	self alarms do:[:alarm| alarm scheduledTime: alarm scheduledTime + deltaTime].