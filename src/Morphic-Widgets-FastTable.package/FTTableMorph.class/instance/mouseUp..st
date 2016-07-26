event handling
mouseUp: event
	| rowIndex |
	
	needToggleAtMouseUp ifFalse: [ ^ self ].
	
	"perform the click if the mouse down didn't did it."
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	needToggleAtMouseUp := false.
	self selectRowIndex: rowIndex event: event