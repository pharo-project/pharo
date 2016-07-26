cycling
selectPreviousTask
	"Make the previous task active."
	
	self selectTask: (self tasks
		before: self activeTask
		ifAbsent: [self tasks isEmpty
					ifFalse: [self tasks last]])