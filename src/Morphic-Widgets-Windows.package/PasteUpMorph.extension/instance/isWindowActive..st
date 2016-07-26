*Morphic-Widgets-Windows
isWindowActive: aSystemWindow
	"Answer whether the given window is active.
	True if the current top window."
	
	^self currentWindow == aSystemWindow