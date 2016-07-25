tests
testAsDateAndTime
	
	| remoteDatetime timestamp |
	timestamp := DosTimestamp on: 16r40B57856.
	self assert: timestamp asDateAndTime equals: '21 May 2012 3:02:44 pm' asDateAndTime.