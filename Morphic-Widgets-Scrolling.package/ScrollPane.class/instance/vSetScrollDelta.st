geometry
vSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta |

	scroller hasSubmorphs ifFalse:[scrollBar interval: 1.0. ^self].
	
	delta := self scrollDeltaHeight.
	range := self vLeftoverScrollRange.
	range = 0 ifTrue: [^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; setValue: 0].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	scrollBar scrollDelta: (delta / range) asFloat 
			pageDelta: ((self innerBounds height - delta) / range) asFloat.
	scrollBar interval: ((self innerBounds height) / self vTotalScrollRange) asFloat.
	scrollBar setValue: (scroller offset y / range min: 1.0) asFloat.
