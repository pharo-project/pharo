*Morphic-Widgets-Taskbar
taskThumbnailOfSize: thumbExtent
	"Answer a new task thumbnail for the receiver."
	
	|min thumb|
	min := self isMinimized
		ifTrue: [self restoreBeforeGeneratingThumbnail.
				true]
		ifFalse: [false].
	thumb := self basicTaskThumbnailOfSize: thumbExtent.
	min ifTrue: [self minimizeAfterGeneratingThumbnail].
	^thumb