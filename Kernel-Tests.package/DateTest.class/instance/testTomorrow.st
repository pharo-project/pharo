tests
testTomorrow

	"Not a great test: could falsely fail if midnight come in between the two executions"
	self assert: Date tomorrow equals: Date today + 1 day.