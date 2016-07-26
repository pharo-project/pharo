status
isSuccess
	"Answers whether the process has been completed successfuly (to our best knowledge)"
	^ self isComplete and: [ self exitStatusInterpreter exitStatus = 0 ]