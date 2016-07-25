composition
composeAllRectangles: rectangles

	| charIndexBeforeLine numberOfLinesBefore reasonForStopping |

	actualHeight := defaultLineHeight.
	charIndexBeforeLine := currCharIndex.
	numberOfLinesBefore := lines size.
	reasonForStopping := self composeEachRectangleIn: rectangles.

	currentY := currentY + actualHeight.
	currentY > theContainer bottom ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex := charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [lines removeLast].
		^self
	].
	
	"It's OK -- the line still fits."
	maxRightX := maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline
	].
	isFirstLine := false.
	reasonForStopping == #columnBreak ifTrue: [^nil].
	currCharIndex > theText size ifTrue: [
		^nil		"we are finished composing"
	].
	