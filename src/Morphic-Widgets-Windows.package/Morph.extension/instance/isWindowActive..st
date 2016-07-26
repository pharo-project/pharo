*Morphic-Widgets-Windows
isWindowActive: aSystemWindow
	"Answer whether the given window is active.
	Implement in morphs that are designed to embed system windows.
	Delegate to owner."
	
	^self owner ifNil: [true] ifNotNil: [:o | o isWindowActive: aSystemWindow]