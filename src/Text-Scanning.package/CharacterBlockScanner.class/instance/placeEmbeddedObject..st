private
placeEmbeddedObject: anchoredMorph
	"Workaround: The following should really use #textAnchorType"
	| w |
	anchoredMorph relativeTextAnchorPosition ifNotNil:[^true].
	w := anchoredMorph width.
	specialWidth := w.
	(destX + w > characterPoint x) ifTrue: [^false].
	destX := destX + w + kern.
	^ true