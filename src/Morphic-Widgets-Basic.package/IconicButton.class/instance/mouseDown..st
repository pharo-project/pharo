mouse events
mouseDown: evt
	"Partial workaraound for broken MouseOverHandler.
	Remove the border on mouse down if mouse focus has changed."

	super mouseDown: evt.
	(actWhen == #buttonDown and: [ evt hand mouseFocus ~= self ])
		ifTrue: [ self mouseLeave: evt ]