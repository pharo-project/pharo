copying
postCopy
	"The copy share same underlying Form but with its own grafPort."

	super postCopy.
	self resetGrafPort