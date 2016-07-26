accessing
dropDownBrick: aBrick
	dropDownBrick ifNotNil: [self removeBrick: dropDownBrick].
	dropDownBrick := aBrick asBrick.
	
	dropDownBrick morph pagerModel: self spotterModel pagerModel.
	self themer spotterThemer dropDownPaneStyleFor: dropDownBrick.
	
	self addBrickBack: dropDownBrick
	"aBrick openInWindow"