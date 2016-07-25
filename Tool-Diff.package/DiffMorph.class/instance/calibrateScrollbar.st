actions
calibrateScrollbar
	"Set the scrollbar parameters to match the texts."

	| maxY range delta innerH |
	self fullBounds.
	maxY := self srcMorph textArea extent y max: self dstMorph extent y.
	innerH := self dstMorph innerBounds extent y.
	delta := 91.	"self dstMorph textMorph defaultLineHeight"
	range := maxY - innerH max: 0.
	range = 0
		ifTrue: [ 
			^ self scrollbarMorph
				scrollDelta: 0.02 pageDelta: 0.2;
				interval: 1.0;
				setValue: 0.0 ].
	self scrollbarMorph
		scrollDelta: (delta / range) asFloat pageDelta: ((innerH - delta) / range) asFloat;
		interval: (innerH / maxY) asFloat;
		setValue:
			((self srcMorph scrollPane scroller offset y max: self dstMorph scrollPane scroller offset y) / range
				min: 1.0) asFloat