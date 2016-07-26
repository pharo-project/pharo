tests
testValueWithinTimingRepeat
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 450 milliSeconds.
