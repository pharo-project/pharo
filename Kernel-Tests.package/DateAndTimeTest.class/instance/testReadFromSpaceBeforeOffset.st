tests - offset
testReadFromSpaceBeforeOffset
	self assert: '2012-07-26 16:38:48 +0200' asDateAndTime offset equals: 2 hour.
	self assert: '2012-07-26 16:38:48 +02' asDateAndTime offset equals: 2 hour.

