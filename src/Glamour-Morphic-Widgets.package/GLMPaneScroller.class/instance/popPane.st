panes
popPane
	transform removeMorph: transform lastSubmorph. "one for the separator"
	transform submorphs isEmpty
		ifFalse: [ transform removeMorph: transform lastSubmorph ].
	paneCount := paneCount - 1.
	self updatePanes.