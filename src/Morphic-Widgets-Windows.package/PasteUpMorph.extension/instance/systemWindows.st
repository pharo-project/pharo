*Morphic-Widgets-Windows
systemWindows
	"Answer the system windows in the world."
	
	^self submorphsSatisfying: [:m | m isSystemWindow]