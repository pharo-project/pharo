enumerating
runsFrom: start to: stop do: aBlock
	"Evaluate aBlock with all existing runs in the range from start to stop"
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue:[:firstRun :offset :firstValue|
		| run value index |
		run := firstRun.
		value := firstValue.
		index := start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run := run + 1.
			value := values at: run.
			index := index + (runs at: run)]].
