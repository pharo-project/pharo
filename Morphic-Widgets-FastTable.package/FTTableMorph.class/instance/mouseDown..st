event handling
mouseDown: event
	| rowIndex |
	"perform the click"
	
	needToggleAtMouseUp ifTrue: [ ^ self ].
	
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	self selectedRowIndex = rowIndex
		ifFalse: [ self selectRowIndex: rowIndex event: event ]
		ifTrue: [ needToggleAtMouseUp := true ].	"If the cell is selected we let the mouse up toggle to avoid any problem with the drag and drop"
	self wantsKeyboardFocus
		ifTrue: [ self takeKeyboardFocus ].
	event hand waitForClicksOrDrag: self event: event