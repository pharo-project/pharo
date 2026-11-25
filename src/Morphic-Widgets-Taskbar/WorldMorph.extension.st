Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Widgets-Taskbar' }
WorldMorph >> newTaskList [
	"Answer a new list of my windows, which I keep"

	| taskList |
	taskList := TaskListMorph new.
	self setProperty: #taskListMorph toValue: taskList.
	^ taskList
]

{ #category : '*Morphic-Widgets-Taskbar' }
WorldMorph >> openTaskListFrom: event [
	"Open my list of windows, or move its selection (backwards with shift)"

	| taskList |
	taskList := self valueOfProperty: #taskListMorph ifAbsent: [ self newTaskList ].
	(taskList hasCurrentWindows: self taskbarTasks) ifFalse: [ taskList := self newTaskList ].
	event shiftPressed
		ifTrue: [ taskList selectPreviousTask ]
		ifFalse: [ taskList selectNextTask ].
	taskList openInWorld: self
]
