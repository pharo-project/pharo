event handling
autoScrollView: evt
	"This is kind of a hack because the PluggableTextMorph expects me to first expand the selection before auto scrolling will work."
	| localEvt |
	localEvt := evt transformedBy: (self transformedFrom: editView).
	super mouseMove: localEvt.
	editView scrollSelectionIntoView: localEvt.