class changes
classChangeAt: className
	"Return what we know about class changes to this class."

	^ (changeRecords at: className ifAbsent: [^ Set new])
		allChangeTypes