instance creation
nowLocal
	"Answer the time since midnight in local timezone"

	| microSecondsToday |
	microSecondsToday := self primUTCMicrosecondsClock.
	^ self
		seconds: microSecondsToday // 1000000 + DateAndTime localTimeZone offset asSeconds \\ SecondsInDay
		nanoSeconds: microSecondsToday \\ 1000000 * 1000