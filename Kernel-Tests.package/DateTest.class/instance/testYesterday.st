tests
testYesterday

	"Not a great test: could falsely fail if midnight come in between the two executions"
	self assert: Date yesterday equals: Date today - 1 day.
