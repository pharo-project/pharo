private
activeTask
	"Answer the active task"
	
	^self tasks detect: [:t | t isActive] ifNone: []