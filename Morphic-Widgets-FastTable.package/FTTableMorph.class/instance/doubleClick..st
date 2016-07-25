event handling
doubleClick: event
	| rowIndex |
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	rowIndex ifNotNil: [ 
		self doAnnounce: (FTStrongSelectionChanged rowIndex: rowIndex event: event) ]