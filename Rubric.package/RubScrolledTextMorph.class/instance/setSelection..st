accessing-selection
setSelection: aSelectionInterval
	aSelectionInterval ifNil: [ ^ self ].
	self setTextAreaSelection: aSelectionInterval.
	self selectionChanged 