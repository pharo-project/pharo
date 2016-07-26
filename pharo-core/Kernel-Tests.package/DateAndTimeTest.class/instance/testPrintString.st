tests
testPrintString
	"self debug: #testPrintString"

	| dt dtNoOffset |
	dt :=DateAndTime
		year: 2004
		month: 11
		day: 2
		hour: 14
		minute: 3
		second: 5
		nanoSecond: 12345
		offset: (Duration seconds: (5 * 3600)).
	self assert: dt printString = '2004-11-02T14:03:05.000012345+05:00'.
	self assert: ('2002-05-16T17:20:45.1+01:01' asDateAndTime printString = '2002-05-16T17:20:45.1+01:01').
	self assert: (' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString = '2002-05-16T17:20:45.02+01:01').
 	self assert: ('2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString =  '2002-05-16T17:20:45.000000009+01:01').
	self assert: ('2002-05-16T17:20:45+00:00' asDateAndTime printString = '2002-05-16T17:20:45+00:00' ).
	self assert: (' 2002-05-16T17:20:45+01:57' asDateAndTime printString = '2002-05-16T17:20:45+01:57').
	self assert: (' 2002-05-16T17:20:45-02:34' asDateAndTime printString = '2002-05-16T17:20:45-02:34').
	self assert: ('2002-05-16T17:20:45+00:00' asDateAndTime printString = '2002-05-16T17:20:45+00:00').
	self assert: ('1997-04-26T01:02:03+01:02:3' asDateAndTime printString = '1997-04-26T01:02:03+01:02:3').
	"When no offset is provided, the local one is used"
	dtNoOffset := '2002-05-16T17:20' asDateAndTime.
	self assert: (('2002-05-16T17:20:00*' match: dtNoOffset printString) and: [dtNoOffset offset = DateAndTime localOffset]).


