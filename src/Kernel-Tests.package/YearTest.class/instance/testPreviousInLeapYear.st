tests
testPreviousInLeapYear
	"self debug: #testPreviousInLeapYear"
	
	| leap |
	leap := Year year: 2008.
	self assert: leap isLeapYear. 
	self assert: (Year year: (leap year - 1)) = leap previous 