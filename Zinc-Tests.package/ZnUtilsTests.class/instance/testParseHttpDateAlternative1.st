testing
testParseHttpDateAlternative1
	"ANSI C's asctime() format"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tue Jan 01 00:00:01 2036'.
	self assert: timeStamp dayOfMonth = 1.
	self assert: timeStamp monthIndex = 1.
	self assert: timeStamp year = 2036.
	self assert: timeStamp hour = 0.
	self assert: timeStamp minute = 0.
	self assert: timeStamp second = 1.
	self assert: timeStamp offset = Duration zero