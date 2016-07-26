*Morphic-Widgets-Taskbar
restoreBeforeGeneratingThumbnail
	"Restore the window without activating unlocking or stepping."
	
	self isMinimized ifFalse: [^self].
	isCollapsed := false.
	self show.
	self setBoundsWithFlex: fullFrame.
	paneMorphs reverseDo: [:m | 
		self addMorph: m].
	self layoutChanged