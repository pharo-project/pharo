initialization
initialize
	super initialize.
	failedList := errorList := Array new.
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded, CategoryAdded, ClassRemoved, CategoryRemoved, ClassRenamed, CategoryRenamed, ClassRecategorized 
			send: #update 
			to: self.
	self update; resetResult