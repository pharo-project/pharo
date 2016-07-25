tests - offset
testReadFromUTCOffset	
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321Z' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+00' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+0000' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+00:00' readStream) offset isZero.

