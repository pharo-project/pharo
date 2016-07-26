stop conditions
wrapAtLastBreakable
	"Wrap the line before last encountered breakable character."
	pendingKernX := 0.
	nextIndexAfterLineBreak := spaceIndex.
	line stop: spaceIndex - 1.
	lineHeight := lineHeightAtSpace.
	baseline := baselineAtSpace.
	line paddingWidth: rightMargin - spaceX.
	line internalSpaces: spaceCount.
	^true