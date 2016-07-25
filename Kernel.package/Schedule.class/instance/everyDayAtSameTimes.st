private
everyDayAtSameTimes
	"Answer false if unknown"
	
	| count |
	count := (Duration days: 1) / self scheduleDuration.
	^ count >= 1 and: [ count isInteger ]