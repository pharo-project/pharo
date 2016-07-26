tests
testFromDos
	
	| aDateAndTime |
	aDateAndTime := DateAndTime fromDosTimestamp: 16r40B57856.
	self assert: aDateAndTime equals:  '21 May 2012 3:02:44 pm' asDateAndTime.
	
	"DOS times are in local time per http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx"
	self assert: aDateAndTime offset equals: DateAndTime localOffset