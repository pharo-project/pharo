event handling
mouseLeaveDragging: evt
	"Handle a mouseLeaveLaden event, meaning the mouse just left my bounds with a button pressed or laden with submorphs. The default response is to let my eventHandler, if any, handle it; else to do nothing."

	self eventHandler ifNotNil:
		[self eventHandler mouseLeaveDragging: evt fromMorph: self]