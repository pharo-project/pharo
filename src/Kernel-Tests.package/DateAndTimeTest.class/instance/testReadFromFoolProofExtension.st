tests
testReadFromFoolProofExtension
	"Convenient extension without a time, only a date"
	"self debug: #testReadFromFoolProofExtension"
	
	self assert: (DateAndTime fuzzyReadFrom: '2008' readStream) printString = '2008-01-01T00:00:00+00:00'.
	self assert: (DateAndTime fuzzyReadFrom: '2008-08' readStream)   printString = '2008-08-01T00:00:00+00:00'.
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28' readStream)  printString = '2006-08-28T00:00:00+00:00'.
	"Regular nanoseconds"
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28T00:00:00.123456789' readStream)  printString = '2006-08-28T00:00:00.123456789+00:00'.
	"Extra picoseconds precision should not spoil the DateAndTime"
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28T00:00:00.123456789000' readStream)  printString = '2006-08-28T00:00:00.123456789+00:00'.