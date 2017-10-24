tests - limits
testMultiSchedule
	"Ensure that scheduling the same delay twice raises an error"
	| delay |
	delay := Delay forSeconds: 1.
	delay schedule.
	self should:[delay schedule] raise: Error.
