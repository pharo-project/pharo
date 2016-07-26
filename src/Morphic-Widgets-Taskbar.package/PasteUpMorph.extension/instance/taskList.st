*Morphic-Widgets-Taskbar
taskList

	^ self submorphs
		detect: [ :e | e isKindOf: TaskListMorph ]
		ifNone: [ NullTaskList in: self ].