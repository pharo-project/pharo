Extension { #name : #PasteUpMorph }

{ #category : #'*Morphic-Widgets-Taskbar' }
PasteUpMorph >> createTaskbarIfNecessary [
	"Private - create a new taskbar if not present."

	self taskbars ifEmpty: [
		TaskbarMorph new openInWorld: self.
		self moveCollapsedWindowsToTaskbar]
]

{ #category : #'*Morphic-Widgets-Taskbar' }
PasteUpMorph >> moveCollapsedWindowsToTaskbar [
	"Move collapsed windows to the taskbar."
	
	(self systemWindows select: [:w | w isCollapsed]) do: [:w |
		w restore; minimize]
]

{ #category : #'*Morphic-Widgets-Taskbar' }
PasteUpMorph >> removeTaskbar [
	"Remove the receiver's taskbars."
	
	self taskbars do: [:each | each removeFromWorld]
]

{ #category : #'*Morphic-Widgets-Taskbar' }
PasteUpMorph >> showWorldTaskbar: aBoolean [
	"Change the receiver to show the taskbar."
	
	aBoolean
		ifTrue: [self createTaskbarIfNecessary]
		ifFalse: [self removeTaskbar]
]

{ #category : #'*Morphic-Widgets-Taskbar' }
PasteUpMorph >> taskList [

	^ self submorphs
		detect: [ :e | e isKindOf: TaskListMorph ]
		ifNone: [ NullTaskList in: self ].
]
