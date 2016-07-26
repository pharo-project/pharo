testing
testParseHttpDateDashesAlternative1
	"RFC 850, obsoleted by RFC 1036"
	
	| timeStamp |
	timeStamp := ZnUtils parseHttpDate: 'Tue, 13-Sep-2011 08:04:49 GMT'.
	self assert: timeStamp dayOfMonth = 13.
	self assert: timeStamp monthIndex = 9.
	self assert: timeStamp year = 2011.
	self assert: timeStamp hour = 8.
	self assert: timeStamp minute = 4.
	self assert: timeStamp second = 49.
	self assert: timeStamp offset = Duration zero