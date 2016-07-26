accessing selection
restoreSelectionIndexesAfter: aBlock
	| prevStart prevStop |
	prevStart := self markIndex min: self pointIndex.
	prevStop := self pointIndex max: self markIndex.
	aBlock value.
	self selectFrom: prevStart to: prevStop - 1