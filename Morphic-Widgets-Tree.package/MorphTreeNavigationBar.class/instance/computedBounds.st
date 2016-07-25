accessing
computedBounds
	| tb tbw yGap |
	tb := treeMorph bounds.
	tbw := treeMorph borderWidth.
	yGap := self layoutInset asPoint y + self cellInset asPoint y.
	^ (tb bottomLeft + (tbw @ (self computedHeight + yGap) negated)) corner: (tb bottomRight - ( (tbw * 2) @ yGap )).
