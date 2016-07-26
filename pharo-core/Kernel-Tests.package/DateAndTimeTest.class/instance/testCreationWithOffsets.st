tests
testCreationWithOffsets
	| dt1 dt2 |
	dt1 := (DateAndTime year: 2222 month: 1 day: 22 hour: 1 minute: 22 second: 33 offset: 0 hours).
	dt2 := (DateAndTime year: 2222 month: 1 day: 22 hour: 1 minute: 22 second: 33 offset: 2 hours).
	
	"The timepoints are diffferent, AKA their UTC times don't correspond"
	self deny: dt1 = dt2.
	
	"The relative components however are equal"
	self assert: dt1 year equals: dt2 year.
	self assert: dt1 month equals: dt2 month.
	self assert: dt1 day equals: dt2 day.
	
	self assert: dt1 hours equals: dt2 hours.
	self assert: dt1 minutes equals: dt2 minutes.
	self assert: dt1 seconds equals: dt2 seconds.