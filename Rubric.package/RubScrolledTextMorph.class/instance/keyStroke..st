event handling
keyStroke: event
	self scrollPane
		ifNotNil: [ :scrollpane | scrollpane keyStroke: (event transformedBy: (scrollpane transformFrom: self)) ]