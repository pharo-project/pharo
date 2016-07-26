geometry
vScrollBarHeight
	| h |

	h := bounds height - (2 * borderWidth).
	self hIsScrollbarNeeded
		ifTrue:[ h := h - self scrollBarThickness. ].
	^h
