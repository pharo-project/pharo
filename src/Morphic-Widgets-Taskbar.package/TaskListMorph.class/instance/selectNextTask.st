cycling
selectNextTask
	"Make the next task active."
	
	self selectTask: (self tasks
		after: self activeTask
		ifAbsent: [self tasks isEmpty
					ifFalse: [self tasks first]])