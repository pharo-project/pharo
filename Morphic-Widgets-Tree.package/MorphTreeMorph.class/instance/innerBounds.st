geometry
innerBounds
	| inner |
	inner := super innerBounds.
	inner := inner withTop: self top + self topHeaderHeight.
	^ inner