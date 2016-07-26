testing
seesNothingOutside: aRectangle
	"Return true if this canvas will not touch anything outside aRectangle"
	^ aRectangle containsRect: self clipRect
