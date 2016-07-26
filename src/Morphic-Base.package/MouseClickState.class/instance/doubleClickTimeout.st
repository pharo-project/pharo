event handling
doubleClickTimeout

	dblClickTimeoutSelector ifNotNil: [
		clickClient perform: dblClickTimeoutSelector with: firstClickDown]