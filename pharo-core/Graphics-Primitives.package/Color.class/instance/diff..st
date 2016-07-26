equality
diff: theOther
	"Returns a number between 0.0 and 1.0"

	^ ((self privateRed - theOther privateRed) abs
		+ (self privateGreen - theOther privateGreen) abs
		+ (self privateBlue - theOther privateBlue) abs)
		/ 3.0 / ComponentMax