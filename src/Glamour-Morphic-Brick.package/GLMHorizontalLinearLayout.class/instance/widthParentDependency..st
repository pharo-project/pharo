width
widthParentDependency: aBrick
	|allowedWidth|
	aBrick owner ifNil: [ Exception signal: 'My width depends on parent, but it is nil' ].
	allowedWidth := aBrick owner subbricks inject: aBrick owner innerBounds width into: [ :width :each |
		each isWidthClean ifTrue: [
			(each layout hState isOnParent not or: [ (self hasWidthRestrictions: each) ])
				ifTrue: [ each layout hasMaxWidth
					ifFalse: [ width - each outerBounds width ]
					ifTrue: [ width ] ]
				ifFalse: [ width ] ]
			ifFalse: [ width ] ].
	^ (allowedWidth * aBrick layout hPercent / 100) ceiling