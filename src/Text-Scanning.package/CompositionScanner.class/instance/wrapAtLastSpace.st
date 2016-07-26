stop conditions
wrapAtLastSpace
	"Wrap the line before last encountered space"
	
	pendingKernX := 0.
	nextIndexAfterLineBreak := spaceIndex + 1.
	alignment = Justified ifTrue: [
		"gobble all subsequent spaces"
		[nextIndexAfterLineBreak <= text size and: [(text at: nextIndexAfterLineBreak) == Space]]
			whileTrue: [nextIndexAfterLineBreak := nextIndexAfterLineBreak + 1]].
	
	line stop: nextIndexAfterLineBreak - 1.
	lineHeight := lineHeightAtSpace.
	baseline := baselineAtSpace.

	["remove the space at which we break..."
	spaceCount := spaceCount - 1.
	spaceIndex := spaceIndex - 1.

	"...and every other spaces preceding the one at which we wrap.
		Double space after punctuation, most likely."
	spaceCount >= 1 and: [(text at: spaceIndex) = Space]]
		whileTrue:
			["Account for backing over a run which might
				change width of space."
			font := text fontAt: spaceIndex withStyle: textStyle.
			spaceX := spaceX - (font widthOf: Space)].
	line paddingWidth: rightMargin - spaceX.
	line internalSpaces: spaceCount.
	^true