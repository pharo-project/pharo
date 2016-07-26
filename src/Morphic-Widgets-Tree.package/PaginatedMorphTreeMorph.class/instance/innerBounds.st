geometry
innerBounds
	| inner |
	inner := super innerBounds.
	inner := inner withBottom: inner bottom - self pagerHeight.
	^ inner