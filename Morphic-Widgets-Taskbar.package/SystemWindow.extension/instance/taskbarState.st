*Morphic-Widgets-Taskbar
taskbarState
	"Answer one of #minimized, #restored, #maximized or #active."

	^self isMinimized
		ifTrue: [#minimized]
		ifFalse: [self isMaximized
			ifTrue: [#maximized]
			ifFalse: [self isActive
						ifTrue: [#active]
						ifFalse: [#restored]]]