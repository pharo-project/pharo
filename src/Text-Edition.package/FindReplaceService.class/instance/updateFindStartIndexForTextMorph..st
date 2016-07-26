updating
updateFindStartIndexForTextMorph: aTextMorph
	| si |
	si := aTextMorph editor selectionInterval.
	self findStartIndex: (self searchBackwards ifTrue: [si first - 1] ifFalse: [si last + 1])