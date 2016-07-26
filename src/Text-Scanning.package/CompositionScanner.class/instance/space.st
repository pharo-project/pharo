stop conditions
space
	"Record left x and character index of the space character just encountered. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the paragraph."

	spaceX := destX.
	spaceIndex := lastIndex.
	lineHeightAtSpace := lineHeight.
	baselineAtSpace := baseline.
	spaceCount := spaceCount + 1.
	lastBreakIsNotASpace := false.
	destX + spaceWidth > rightMargin ifTrue:[^self crossedX].
	destX := spaceX + spaceWidth + kern.
	lastIndex := lastIndex + 1.
	^false
