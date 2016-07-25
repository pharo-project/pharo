*Morphic-Widgets-Taskbar
worldTaskbar
	"Answer the world taskbar or nil if none."

	^self world ifNotNil: [ :world | 
		world 
		submorphThat: [ :morph | morph isTaskbar ] 
		ifNone: [ nil ]
	] 