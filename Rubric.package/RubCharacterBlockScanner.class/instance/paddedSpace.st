stop conditions
paddedSpace
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	pad := 0.
	spaceCount := spaceCount + 1.
	pad := line justifiedPadFor: spaceCount font: font.
	lastSpaceOrTabExtent := lastCharacterExtent copy.
	self lastSpaceOrTabExtentSetX:  spaceWidth + pad.
	(destX + lastSpaceOrTabExtent x)  >= characterPoint x
		ifTrue: [lastCharacterExtent := lastSpaceOrTabExtent copy.
				^self crossedX].
	lastIndex := lastIndex + 1.
	destX := destX + lastSpaceOrTabExtent x.
	^ false
