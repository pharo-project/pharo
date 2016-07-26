scrollbars
vPageDelta
	"Answer the vertical page delta."
	
	|pd tw sw|
	tw := self scrollTarget height.
	sw := self scrollBounds height.
	pd := tw - sw  max: 0.
	pd = 0 ifFalse: [pd := sw / pd].
	^pd