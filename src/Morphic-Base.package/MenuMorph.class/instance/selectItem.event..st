control
selectItem: aMenuItem event: anEvent
	selectedItem ifNotNil:[selectedItem deselect: anEvent].
	selectedItem := aMenuItem.
	selectedItem ifNotNil:[selectedItem select: anEvent].