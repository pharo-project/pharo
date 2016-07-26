*Morphic-Widgets-Windows
isInSystemWindow
	"answer if the receiver is in a system window"
	^ owner isMorph and:[owner isSystemWindow or:[owner isInSystemWindow]]