double click support
waitForClicksOrDrag: aMorph event: evt
	"Wait for mouse button and movement events, informing aMorph about events interesting to it via callbacks.
	This message is typically sent to the Hand by aMorph when it first receives a mouse-down event.
	The callback methods invoked on aMorph (which are passed a copy of evt) are:
		#click:	sent when the mouse button goes up within doubleClickTime.
		#doubleClick:	sent when the mouse goes up, down, and up again all within DoubleClickTime.
		#doubleClickTimeout:  sent when the mouse does not have a doubleClick within DoubleClickTime.
		#startDrag:	sent when the mouse moves more than 10 pixels from evt's position within DoubleClickTime.
	Note that mouseMove: and mouseUp: events are not sent to aMorph until it becomes the mouse focus,
	which is typically done by aMorph in its click:, doubleClick:, or drag: methods."
	
	^self waitForClicksOrDrag: aMorph event: evt selectors: #( #click: #doubleClick: #doubleClickTimeout: #startDrag:) threshold: 10
