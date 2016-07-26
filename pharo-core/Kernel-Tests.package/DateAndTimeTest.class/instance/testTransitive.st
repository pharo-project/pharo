tests - under design
testTransitive
	"self debug: #testTransitive"
	| t1 t2 t3 |
	t1 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	t2 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	t3 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	self assert: ((t1 = t2) & (t2 = t3) ==> (t1 = t3)).
	
	
