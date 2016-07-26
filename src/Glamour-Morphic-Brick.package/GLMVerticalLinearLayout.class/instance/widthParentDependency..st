width
widthParentDependency: aBrick
	
	aBrick owner ifNil: [ Exception signal: 'My width depends on parent, but it is nil' ].
	^ (aBrick owner innerBounds width * aBrick layout hPercent / 100) ceiling