*Morphic-Base
waitForClicksOrDrag: aMorph event: evt selectors: clickAndDragSelectors threshold: threshold

	"Wait for mouse button and movement events, informing aMorph about events interesting to it via callbacks.
	This message is typically sent to the Hand by aMorph when it first receives a mouse-down event.
	The callback methods, named in clickAndDragSelectors and passed a copy of evt, are:
		1 	(click) sent when the mouse button goes up within doubleClickTime.
		2	(doubleClick) sent when the mouse goes up, down, and up again all within DoubleClickTime.
		3	(doubleClickTimeout) sent when the mouse does not have a doubleClick within DoubleClickTime.
		4	(startDrag) sent when the mouse moves more than threshold pixels from evt's position within DoubleClickTime.
	Note that mouseMove: and mouseUp: events are not sent to aMorph until it becomes the mouse focus,
	which is typically done by aMorph in its click:, doubleClick:, or drag: methods."
	
	mouseClickState := 
		MouseClickState new
			client: aMorph 
			click: clickAndDragSelectors first 
			dblClick: clickAndDragSelectors second 
			dblClickTime: DoubleClickTime 
			dblClickTimeout: clickAndDragSelectors third
			drag: clickAndDragSelectors fourth 
			threshold: threshold 
			event: evt.
