width
heightParentDependency: aBrick
	| allowedHeight |
	aBrick owner ifNil: [ Exception signal: 'My height depends on parent, but it is nil' ].
	allowedHeight := aBrick owner subbricks inject: aBrick owner innerBounds height into: [ :height :each |
		each isHeightClean ifTrue: [
			(each layout vState isOnParent not | (self hasHeightRestrictions: each))
				ifTrue: [ height - each outerBounds height ]
				ifFalse: [ height ] ]
			ifFalse: [ height ] ].
	^ (allowedHeight * aBrick layout vPercent / 100) ceiling