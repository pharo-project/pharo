user interface
doFastReframe: ptName

	| newBounds |
	"For fast display, only higlight the rectangle during loop"
	newBounds := self boundsInWorld newRectButtonPressedDo: [:rectangle :cursorPoint | 
		rectangle
			withSideOrCorner: ptName
			setToPoint: cursorPoint
			minExtent: self minimumExtent].
	Display deferUpdatesIn: Display boundingBox while: [
		self bounds: newBounds].

	^newBounds.