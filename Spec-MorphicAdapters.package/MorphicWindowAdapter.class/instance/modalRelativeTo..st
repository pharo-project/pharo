spec protocol
modalRelativeTo: aWindow

	self widgetDo: [ :w | aWindow setModal: w ]