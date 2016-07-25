instance creation
nowUTC
	"Answer the time since midnight in UTC timezone"

	| microSecondsToday |
	microSecondsToday := self primUTCMicrosecondsClock \\ MicrosecondsInDay.
	^ self 
		seconds: microSecondsToday // 1000000 
		nanoSeconds: microSecondsToday \\ 1000000 * 1000