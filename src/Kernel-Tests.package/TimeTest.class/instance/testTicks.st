tests
testTicks
	self assert: aTime ticks = #(0 45296 0).
	self assert: aTime  = (Time new ticks: #(0 45296 0))