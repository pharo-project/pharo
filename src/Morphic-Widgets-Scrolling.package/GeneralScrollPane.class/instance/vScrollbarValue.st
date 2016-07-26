scrollbars
vScrollbarValue
	"Answer the computed vertical scrollbar value."
	
	|tw sw v|
	tw := self scrollTarget height.
	sw := self scrollBounds height.
	v := tw - sw  max: 0.
	v = 0 ifFalse: [v := self scroller offset y asFloat / v min: 1.0].
	^v