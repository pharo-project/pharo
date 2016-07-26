tests
testValueWithinTimingNestedInner
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.
