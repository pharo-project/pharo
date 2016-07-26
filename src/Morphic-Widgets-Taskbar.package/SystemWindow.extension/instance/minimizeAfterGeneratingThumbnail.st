*Morphic-Widgets-Taskbar
minimizeAfterGeneratingThumbnail
	"Minimize the window after thumbnail generation."
	
	self isMinimized ifTrue: [^self].
	isCollapsed := true.
	paneMorphs
		do: [:m | m delete; releaseCachedState].
	self setBoundsWithFlex: (-10 @ -10 extent: 2 @ 2).
	self hide.
	self layoutChanged
	