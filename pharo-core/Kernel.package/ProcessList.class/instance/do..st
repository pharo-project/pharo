enumerating
do: aBlock

	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink value.
		 aLink := aLink nextLink]