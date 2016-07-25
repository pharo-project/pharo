scrollbars
hPageDelta
	"Answer the horizontal page delta."
	
	|pd tw sw|
	tw := self scrollTarget width.
	sw := self scrollBounds width.
	pd := tw - sw  max: 0.
	pd = 0 ifFalse: [pd := sw / pd].
	^pd