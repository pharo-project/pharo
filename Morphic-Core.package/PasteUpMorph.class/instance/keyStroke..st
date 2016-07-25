event handling
keyStroke: anEvent
	"A keystroke has been made.  Service event handlers and, if it's a keystroke presented to the world, dispatch it to #unfocusedKeystroke:"

	super keyStroke: anEvent.	"Give event handlers a chance"
	self selectedObject ifNotNil: [ :selected | selected moveOrResizeFromKeystroke: anEvent ].
	self dispatchKeystroke: anEvent