evaluating
timeToRun
	"Answer the number of milliseconds taken to execute this block."

	^ Duration milliSeconds: (Time millisecondsToRun: self)