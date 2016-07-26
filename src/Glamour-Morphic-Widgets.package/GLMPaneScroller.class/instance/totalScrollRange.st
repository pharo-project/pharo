scrolling
totalScrollRange
	| submorphBounds |
	submorphBounds := transform localSubmorphBounds ifNil: [^ 0].
	^ submorphBounds width
