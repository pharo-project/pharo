enumerating
linksDo: aBlock

	| aLink |
	aLink := firstLink.
	[aLink == nil ] whileFalse:
		[
		aBlock value: aLink.
		aLink := aLink nextLink]