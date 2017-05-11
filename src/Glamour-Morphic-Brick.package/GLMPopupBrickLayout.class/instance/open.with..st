opening
open: aPopupBrick with: aRectangle
	"Open a given popup brick so that a trigger brick is located just above the provided
	rectangle in global coordinates. My subclasses should implement a concrete strategy based
	on layout direction (downward or upward).
	Return a morphic foundation brick used to wrap a popup"
	<return: #GLMFoundationBrick>

	^ self subclassResponsibility