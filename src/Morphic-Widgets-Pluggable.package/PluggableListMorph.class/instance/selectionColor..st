accessing
selectionColor: aColor
	"Set the colour for selected items."

	| w |
	selectionColor := aColor.
	w := self ownerThatIsA: SystemWindow.
	self selectionColorToUse:
			((self theme settings fadedBackgroundWindows not or: [ w isNil or: [ w isActive ] ])
				ifTrue: [ aColor ]
				ifFalse: [ self theme unfocusedSelectionColor ])