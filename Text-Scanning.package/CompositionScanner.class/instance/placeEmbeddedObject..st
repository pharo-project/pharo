private
placeEmbeddedObject: anchoredMorph
	| w descent |
	"Workaround: The following should really use #textAnchorType"
	anchoredMorph relativeTextAnchorPosition ifNotNil:[^true].
	w := anchoredMorph width.
	(destX + w > rightMargin and: [(leftMargin + w) <= rightMargin or: [lastIndex > line first]])
		ifTrue: ["Won't fit, but would on next line"
				^ false].
	destX := destX + w + kern.
	descent := lineHeight - baseline.
	baseline := baseline max: anchoredMorph height.
	lineHeight := baseline + descent.
	^ true