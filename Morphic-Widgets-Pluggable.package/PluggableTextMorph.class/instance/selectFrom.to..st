interactive error protocol
selectFrom: start to: stop
	self textMorph editor selectFrom: start to: stop.
	self selectionInterval: self textMorph editor selectionInterval.
	^ self selectionInterval