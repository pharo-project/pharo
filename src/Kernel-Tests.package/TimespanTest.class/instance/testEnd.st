tests
testEnd
	"self debug: #testEnd"
	self assert: aTimespan end + (Duration  nanoSeconds: 1) =  aDisjointTimespan start.
	self assert: aTimespan end = (DateAndTime year: 2005 month: 1 day: 7 hour: 23 minute: 59 second: 59 nanoSecond: 999999999 offset: 0 hours).
	

