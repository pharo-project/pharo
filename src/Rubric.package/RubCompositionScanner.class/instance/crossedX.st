stop conditions
crossedX
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition."

	pendingKernX := 0.
	breakAtSpace ifTrue: [
		spaceCount >= 1 ifTrue:
			["The common case. First back off to the space at which we wrap."
			line stop: breakableIndex.
			lineHeight := lineHeightAtBreak.
			baseline := baselineAtBreak.
			spaceCount := spaceCount - 1.
			breakableIndex := breakableIndex - 1.

			"Check to see if any spaces preceding the one at which we wrap.
				Double space after punctuation, most likely."
			[(spaceCount > 1 and: [(text at: breakableIndex) = Character space])]
				whileTrue:
					[spaceCount := spaceCount - 1.
					"Account for backing over a run which might
						change width of space."
					font := text fontAt: breakableIndex withStyle: textStyle.
					breakableIndex := breakableIndex - 1.
					spaceX := spaceX - (font widthOf: Character space)].
			line paddingWidth: rightMargin - spaceX.
			line internalSpaces: spaceCount]
		ifFalse:
			["Neither internal nor trailing spaces -- almost never happens."
			lastIndex := lastIndex - 1.
			[destX <= rightMargin]
				whileFalse:
					[destX := destX - (font widthOf: (text at: lastIndex)).
					lastIndex := lastIndex - 1].
			spaceX := destX.
			line paddingWidth: rightMargin - destX.
			line stop: (lastIndex max: line first)].
		^true
	].

	(breakableIndex isNil or: [breakableIndex < line first]) ifTrue: [
		"Any breakable point in this line.  Just wrap last character."
		breakableIndex := lastIndex - 1.
		lineHeightAtBreak := lineHeight.
		baselineAtBreak := baseline.
	].

	"It wasn't a space, but anyway this is where we break the line."
	line stop: breakableIndex.
	lineHeight := lineHeightAtBreak.
	baseline := baselineAtBreak.
	^ true.
