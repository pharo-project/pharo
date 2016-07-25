tests - under design
testNotSymmetricWithString
	"self debug: #testNotSymmetricWithString"
	
	| t1 t2 |
	t1 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	t2 :=  '0001-01-02T02:03:04.000000005+06:00'.
	self deny: t1 = t2.
	self deny: t2 = t1.
	
	
