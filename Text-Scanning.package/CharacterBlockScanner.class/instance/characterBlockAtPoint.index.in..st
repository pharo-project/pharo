scanning
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder.  It combines
	MVC's characterBlockAtPoint:, -ForIndex:, and buildCharacterBlockIn:"
	| runLength lineStop stopCondition |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	self setStopConditions.		"also sets font"
	characterIndex := index.  " == nil means scanning for point"
	characterPoint := aPoint.
	(characterPoint isNil or: [characterPoint y > line bottom])
		ifTrue: [characterPoint := line bottomRight].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	(text isEmpty or: [(characterPoint y < destY or: [characterPoint x < destX])
				or: [characterIndex notNil and: [characterIndex < line first]]])
		ifTrue:	[^ (CharacterBlock new stringIndex: line first text: text
					topLeft: destX@destY extent: 0 @ textStyle lineGrid)
					textLine: line].
	runLength := text runLengthFor: line first.
	lineStop := characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.

	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x.
		"see setStopConditions for stopping conditions for character block operations."
		self perform: stopCondition
	] whileFalse.
	characterIndex
		ifNil: ["Result for characterBlockAtPoint: "
				^ (CharacterBlock new
					stringIndex: lastIndex
					text: text topLeft: characterPoint + (font descentKern @ 0)
					extent: lastCharacterWidth @ line lineHeight - (font baseKern @ 0))
							textLine: line]
		ifNotNil: ["Result for characterBlockForIndex: "
				^ (CharacterBlock new
					stringIndex: characterIndex
					text: text topLeft: characterPoint + ((font descentKern) - kern @ 0)
					extent: lastCharacterWidth @ line lineHeight)
							textLine: line]