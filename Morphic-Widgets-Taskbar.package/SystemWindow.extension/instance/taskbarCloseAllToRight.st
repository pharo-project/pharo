*Morphic-Widgets-Taskbar
taskbarCloseAllToRight
	| wasFound |
	wasFound := false.
	self worldTaskbar ifNotNil: [ :worldTaskbar |
		worldTaskbar orderedTasks copy do: [ :task | 
			wasFound 
				ifTrue:  [ task morph delete ] 
				ifFalse: [ wasFound := task morph == self ].
			]. 
		].