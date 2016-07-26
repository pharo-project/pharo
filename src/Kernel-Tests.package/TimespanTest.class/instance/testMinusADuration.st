tests
testMinusADuration
	"It appears that subtracting a duration from a Timespan gives you a Timespan shifted by the duration"
	self assert: aTimespan - aDay =  anOverlappingTimespan.
	self assert: aDisjointTimespan - aWeek =  aTimespan.	


