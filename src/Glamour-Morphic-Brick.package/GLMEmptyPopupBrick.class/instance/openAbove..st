popup - opening dispatch
openAbove: aRectangle
	"aRectangle must be in global coordinates"
	| anOrigin |
	
	anOrigin := aRectangle origin - (0 @ self contentBrick height) - ( 0 @ self inset top).
	^ self openAt: anOrigin