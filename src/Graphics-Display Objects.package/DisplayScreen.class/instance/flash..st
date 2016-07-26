displaying
flash: aRectangle 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceDisplayUpdate.
