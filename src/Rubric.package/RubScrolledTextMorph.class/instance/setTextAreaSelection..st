accessing-selection
setTextAreaSelection: aSelectionInterval
	aSelectionInterval ifNil: [ ^ self ].
	self textArea selectFrom: aSelectionInterval first to: aSelectionInterval last.
	self scrollPane scrollSelectionIntoView