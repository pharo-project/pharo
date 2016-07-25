geometry
hSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta |

	scroller hasSubmorphs ifFalse:[scrollBar interval: 1.0. ^self].
	
	delta := self scrollDeltaWidth.
	range := self hLeftoverScrollRange.
	range = 0 ifTrue: [ hScrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; setValue: 0. ^self].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."

	hScrollBar 
			scrollDelta: (delta / range) asFloat 
			pageDelta: ((self innerBounds width - delta) / range) asFloat.
	hScrollBar interval: ((self innerBounds width) / self hTotalScrollRange) asFloat.
	hScrollBar setValue: ((scroller offset x / range) min: 1.0) asFloat.
