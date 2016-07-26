text attributes
setActualFont: aFont
	"Keep track of max height and ascent for auto lineheight"
	| descent |
	super setActualFont: aFont.
	lineHeight == nil
		ifTrue: [descent := font descent.
				baseline := font ascent.
				lineHeight := baseline + descent]
		ifFalse: [descent := lineHeight - baseline max: font descent.
				baseline := baseline max: font ascent.
				lineHeight := lineHeight max: baseline + descent]