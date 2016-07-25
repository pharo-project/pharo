drawing
boundingBoxOfSubmorphs
	| aBox visibleSubmorphs |
	visibleSubmorphs := submorphs select: [ :m | m visible ].
	visibleSubmorphs ifEmpty: [ ^ bounds origin extent: self minimumExtent "so won't end up with something empty" ].
	aBox := visibleSubmorphs first fullBounds.
	visibleSubmorphs allButFirst do: [ :m | aBox := aBox quickMerge: m fullBounds ].
	^ aBox