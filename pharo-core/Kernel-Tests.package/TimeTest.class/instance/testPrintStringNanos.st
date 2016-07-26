tests
testPrintStringNanos
	self assert: (Time hour: 15 minute: 15 second: 15  nanoSecond: (150 *
1000000)) printString = '3:15:15.15 pm'