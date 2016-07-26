scrollpane-events-mouse
handleListenEvent: anEvent
	"checking if it's keyboard event (mouse wheel is keyboard event on windows o_O (wtf?)"
	anEvent type = #keystroke ifTrue: [
		"checking if mouse is inside me"
		((self boundsInWorld containsPoint: anEvent position)) ifTrue: [
			"actuall scrolling"
			self scrollByKeyboard: anEvent ] ]