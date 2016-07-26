initialize
intoWorld: aWorld

	updateTask isTerminated ifTrue: [ self setTaskAction ].

	super intoWorld: aWorld