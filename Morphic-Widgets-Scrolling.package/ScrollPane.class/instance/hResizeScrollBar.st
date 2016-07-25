geometry
hResizeScrollBar

	| topLeft h |

	(self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]) ifTrue: [^self].
	bounds ifNil: [ self fullBounds ].
	
	h := self scrollBarThickness.
	topLeft := bounds bottomLeft + (borderWidth @ (h + borderWidth) negated).
	hScrollBar bounds: (topLeft extent: self hScrollBarWidth@ h)
