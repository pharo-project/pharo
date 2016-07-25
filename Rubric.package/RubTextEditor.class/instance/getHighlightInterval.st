accessing-selection
getHighlightInterval
	"Answer the interval that is or should be highligted."

	^ self hasSelection 
		ifTrue: [ self selectionInterval ]
		ifFalse: [ self computeSelectionIntervalForCurrentLine ]