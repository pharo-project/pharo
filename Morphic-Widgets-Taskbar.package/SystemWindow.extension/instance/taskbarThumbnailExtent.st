*Morphic-Widgets-Taskbar
taskbarThumbnailExtent
	"Answer the size of a taskbar thumbnail for the receiver."

	^self isMinimized
		ifTrue: [self fullFrame extent min: self defaultTaskbarThumbnailExtent]
		ifFalse: [super taskbarThumbnailExtent]