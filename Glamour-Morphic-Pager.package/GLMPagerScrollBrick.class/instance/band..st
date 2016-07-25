accessing
band: aBrick

	band := aBrick.
	band
		useHorizontalLinearLayout;
		layoutMorphs: true;
		width: [ self band subbricksCount * self width / ((self maxPanes min: self band subbricksCount) max: 1) ];
		vSpaceFill.
	"band actually depends from myself,
	because each pane is proportional to my size"
	band layout widthDependency: self.
	
	self addBrickBack: band.