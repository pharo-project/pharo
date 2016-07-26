scrollpane-brick-accessing
verticalScrollbar: aBrick

	verticalScrollbar := aBrick.
	verticalScrollbar scrollPane: self.
	verticalScrollbar height: [ self height ].
	verticalScrollbar layout heightDependency: self.
	super addBrickBack: verticalScrollbar