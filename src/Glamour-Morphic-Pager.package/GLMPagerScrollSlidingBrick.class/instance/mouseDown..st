event-mouse
mouseDown: evt

	startDragX := (evt position x - self globalBounds left).
	startResizingWidth := self width.
	
	isRightResizing := self isRightResizingZone: evt.
	
	"only one type of resizing is possible"
	isRightResizing
		ifFalse: [ isLeftResizing := self isLeftResizingZone: evt ]
		ifTrue: [ isLeftResizing := false ].

	isResizing := (isLeftResizing or: isRightResizing).
		
	super mouseDown: evt.