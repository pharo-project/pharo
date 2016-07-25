tests - under design
testReadFromSecond
	"self debug: #testReadFromSecond"
	
	self assert: ('-1199-01-05T20:33:14.321-05:00' asDateAndTime printString = '-1199-01-05T20:33:14.321-05:00').
	self assert: ('2002-05-16T17:20:45.1+01:01' asDateAndTime printString = '2002-05-16T17:20:45.1+01:01').
	self assert: (' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString = '2002-05-16T17:20:45.02+01:01').  
	self assert: ('2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString =  '2002-05-16T17:20:45.000000009+01:01').
	self assert: (' 2002-05-16T17:20' asDateAndTime translateToUTC printString = '2002-05-16T17:20:00+00:00').
	self assert: ('2002-05-16T17:20:45' asDateAndTime translateToUTC printString = '2002-05-16T17:20:45+00:00' ).
	self assert: (' 2002-05-16T17:20:45+01:57' asDateAndTime printString = '2002-05-16T17:20:45+01:57').
	self assert: (' 2002-05-16T17:20:45-02:34' asDateAndTime printString = '2002-05-16T17:20:45-02:34').
	self assert: ('2002-05-16T17:20:45+00:00' asDateAndTime printString = '2002-05-16T17:20:45+00:00').
	self assert: ('1997-04-26T01:02:03+01:02:3' asDateAndTime printString = '1997-04-26T01:02:03+01:02:3'). 