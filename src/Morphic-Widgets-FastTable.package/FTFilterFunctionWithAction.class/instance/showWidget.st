accessing
showWidget
	super showWidget.
	table addMorph: actionButton.
	table selectedRowIndex = 0
		ifTrue: [ table selectRowIndex: 1 ]