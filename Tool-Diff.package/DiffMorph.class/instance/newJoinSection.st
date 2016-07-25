instance-creation
newJoinSection
	"Answer a new join section."

	^self joinSectionClass new
		srcColor: self modificationColor;
		dstColor: self modificationColor;
		borderWidth: 1;
		borderColor: self edgeColor;
		addDependent: self;
		yourself