alarms
removeAlarm: aSelector for: aTarget
	"Remove the alarm with the given selector"

	self alarms
		detect: [ :any | any receiver == aTarget and: [ any selector == aSelector ] ]
		ifFound: [ :alarm | self alarms remove: alarm ]