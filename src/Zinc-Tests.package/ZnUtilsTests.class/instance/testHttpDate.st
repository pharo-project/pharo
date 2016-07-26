testing
testHttpDate
	| timestamp string |
	timestamp := (DateAndTime year: 2010 month: 9 day: 1 hour: 10 minute: 10 second: 10 offset: 0 seconds).
	string := ZnUtils httpDate: timestamp.
	self assert: (string = 'Wed, 01 Sep 2010 10:10:10 GMT')
	