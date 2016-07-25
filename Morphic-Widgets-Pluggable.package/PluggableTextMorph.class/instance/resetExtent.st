geometry
resetExtent
	"Reset the extent while maintaining the current selection.  Needed when resizing while the editor is active (when inside the pane)."
	| tempSelection |
	textMorph notNil ifTrue:
		["the current selection gets munged by resetting the extent, so store it"
		tempSelection := self selectionInterval.
		super resetExtent. "adjust scroller"
		self extent: self extent.
		self setSelection: tempSelection]