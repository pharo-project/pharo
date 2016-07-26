tests
testValueWithinTimingBasic
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.