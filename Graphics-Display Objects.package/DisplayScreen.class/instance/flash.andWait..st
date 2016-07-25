displaying
flash: aRectangle andWait: msecs
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
