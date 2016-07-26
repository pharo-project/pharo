scanning
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide
	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition |
	"Set up margins"
	leftMargin := lineRectangle left.
	leftSide ifTrue: [leftMargin := leftMargin +
						(firstLine ifTrue: [textStyle firstIndent]
								ifFalse: [textStyle restIndent])].
	destX := spaceX := leftMargin.
	firstDestX := destX.
	rightMargin := lineRectangle right.
	rightSide ifTrue: [rightMargin := rightMargin - textStyle rightIndent].
	lastIndex := startIndex.	"scanning sets last index"
	destY := lineRectangle top.
	lineHeight := baseline := 0.  "Will be increased by setFont"
	self setStopConditions.	"also sets font"
	runLength := text runLengthFor: startIndex.
	runStopIndex := (lastIndex := startIndex) + (runLength - 1).
	line := (RubTextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	spaceCount := 0.
	self handleIndentation.
	leftMargin := destX.
	line leftMargin: leftMargin.

	[false]
		whileFalse: 
			[stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: kern.
			"See setStopConditions for stopping conditions for composing."
			line stopCondition: stopCondition.
			(self perform: stopCondition)
				ifTrue: [^ line lineHeight: lineHeight + textStyle leading baseline: baseline + textStyle leading]]