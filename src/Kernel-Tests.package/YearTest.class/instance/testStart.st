tests
testStart
	"self debug: #testStart"
	
	| yyyy |
	yyyy := DateAndTime now year.
	self assert: Year current start = (DateAndTime year: yyyy month: 1 day: 1)