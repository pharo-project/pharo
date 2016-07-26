*Morphic-Widgets-Taskbar
taskbarTask
	"Answer a new taskbar task for the receiver.
	Answer nil if not required."

	(self valueOfProperty: #noTaskbarTask ifAbsent: [false]) ifTrue: [^nil].
	^TaskbarTask
		morph: self
		state: self taskbarState
		icon: self taskbarIcon
		label: self taskbarLabel