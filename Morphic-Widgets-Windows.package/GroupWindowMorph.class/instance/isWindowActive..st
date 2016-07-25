testing
isWindowActive: aSystemWindow
	"Answer whether the given window is active.
	True if the receiver is active and the window is the
	current page."
	
	^ self tabGroup page == aSystemWindow and: [aSystemWindow isTopWindow ]