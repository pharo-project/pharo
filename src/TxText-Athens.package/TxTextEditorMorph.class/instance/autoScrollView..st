scrolling
autoScrollView: evt
	"This is kind of a hack because the PluggableTextMorph expects me to first expand the selection before auto scrolling will work."

	evt position y < self top
		ifTrue: [ layoutViewMorph scrollBy: 0 @ (evt position y - self top) abs negated ]
		ifFalse: [ layoutViewMorph scrollBy: 0 @ (evt position y - self top) abs ].
	
	self selectTo: evt position - layoutViewMorph innerBounds origin.