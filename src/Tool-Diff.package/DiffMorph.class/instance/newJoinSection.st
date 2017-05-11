instance-creation
newJoinSection
	"Answer a new join section."

	^self joinSectionClass new
		srcColor: self modificationColor;
		dstColor: self modificationColor;
		borderWidth: 1;
		borderColor: self edgeColor;
		additionHighlightColor: self additionHighlightColor;
		removalHighlightColor: self removalHighlightColor;
		addDependent: self;
		yourself