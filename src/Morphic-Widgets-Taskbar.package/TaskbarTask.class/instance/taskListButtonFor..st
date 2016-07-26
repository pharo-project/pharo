user-interface
taskListButtonFor: aTasklist
	"Answer a button for the task."
	
	^self theme
		newTaskListButtonIn: aTasklist
		for: self