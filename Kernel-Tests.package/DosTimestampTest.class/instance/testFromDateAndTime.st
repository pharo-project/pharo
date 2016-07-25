tests
testFromDateAndTime

	| aDateAndTime timestamp |
	timestamp := DosTimestamp fromDateAndTime: '21 May 2012 3:02:44 pm' asDateAndTime.
	
	self assert: timestamp value equals: 16r40B57856.