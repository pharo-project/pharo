width
widthParentDependency: aBrick
	|allowedWidth|
	aBrick owner ifNil: [ Exception signal: 'My width depends on parent, but it is nil' ].
	allowedWidth := aBrick owner subbricks inject: aBrick owner innerBounds width into: [ :width :each |
		each isWidthClean ifTrue: [
			(each layout hState isOnParent not | (self hasWidthRestrictions: each))
				ifTrue: [ width - each outerBounds width ]
				ifFalse: [ width ] ]
			ifFalse: [ width ] ].
	^ (allowedWidth * aBrick layout hPercent / 100) ceiling