stop conditions
wrapHere
	"Wrap the line before current character."
	pendingKernX := 0.
	nextIndexAfterLineBreak := lastIndex.
	lastIndex := lastIndex - 1.
	spaceX := destX.
	line paddingWidth: rightMargin - destX.
	line stop: (lastIndex max: line first).
	^true