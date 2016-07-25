scrolling
vUnadjustedScrollRange
"Return the width of the widest item in the list"

	(scroller submorphs size > 0) ifFalse:[ ^0 ].
	^scroller submorphs last fullBounds bottom
