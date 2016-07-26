geometry testing
hIsScrollable

	"If the contents of the pane are too small to scroll, return false."
	^ self hLeftoverScrollRange > 0
	
