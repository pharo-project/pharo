accessing
clipRect: aRectangle 
	"Set the receiver's clipping area rectangle to be the argument, aRectangle."
	clipX := aRectangle left truncated.
	clipY := aRectangle top truncated.
	clipWidth := aRectangle right truncated - clipX.
	clipHeight := aRectangle bottom truncated - clipY