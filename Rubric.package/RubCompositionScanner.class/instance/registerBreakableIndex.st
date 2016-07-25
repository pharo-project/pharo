multilingual scanning
registerBreakableIndex

	"Record left x and character index of the line-wrappable point. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the paragraph."

	(text at: lastIndex) = Character space ifTrue: [
		breakAtSpace := true.
		spaceX := destX.
		spaceCount := spaceCount + 1.
		lineHeightAtBreak := lineHeight.
		baselineAtBreak := baseline.
		breakableIndex := lastIndex.
		destX > rightMargin ifTrue: 	[^self crossedX].
	] ifFalse: [
		breakAtSpace := false.
		lineHeightAtBreak := lineHeight.
		baselineAtBreak := baseline.
		breakableIndex := lastIndex - 1.
	].
	^ false.
