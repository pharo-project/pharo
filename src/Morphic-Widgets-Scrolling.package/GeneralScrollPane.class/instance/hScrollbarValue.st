scrollbars
hScrollbarValue
	"Answer the computed horizontal scrollbar value."
	
	|tw sw v|
	tw := self scrollTarget width.
	sw := self scrollBounds width.
	v := tw - sw  max: 0.
	v = 0 ifFalse: [v :=  self scroller offset x asFloat / v min: 1.0].
	^v