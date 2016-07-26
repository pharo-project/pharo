status
isRunning
	"Answers whether the process is still running or not (to our best knowledge)"
	^ pid notNil and: [ self isComplete not ]