event-listening
logClassRenamed: annoucement
	annoucement classRenamed acceptsLoggingOfCompilation ifTrue: [
		self logChange: '(Smalltalk globals at: #', annoucement oldName, ') rename: #', annoucement newName.	
	].
