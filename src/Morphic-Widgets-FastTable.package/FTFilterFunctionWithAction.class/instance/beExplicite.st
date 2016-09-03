accessing
beExplicite
	super beExplicite.
	table addMorph: actionButton.
	table selectedRowIndex = 0
		ifTrue: [ table selectRowIndex: 1 ]