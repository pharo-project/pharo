private - access
activeTab: aTabMorph

	self activeTab ifNotNil: [self activeTab toggle].
	activeTab := aTabMorph.
	self activeTab toggle.
	aTabMorph delete.
	self addMorphFront: aTabMorph.
	aTabMorph takeKeyboardFocus.
	self performActiveTabAction.
	self changed.
