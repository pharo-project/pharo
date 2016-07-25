tests
testAsDos
	
	| remoteDatetime |
	self assert: '21 May 2012 3:02:44 pm' asDateAndTime asDosTimestamp equals: 16r40B57856.

	"DOS times are in local time per http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx"
	remoteDatetime := DateAndTime current offset: DateAndTime localOffset + 2 hours.
	self assert: remoteDatetime asDosTimestamp equals: remoteDatetime asLocal asDosTimestamp.