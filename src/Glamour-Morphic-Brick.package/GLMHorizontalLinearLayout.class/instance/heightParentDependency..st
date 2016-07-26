height
heightParentDependency: aBrick
	"if aBrick depends on parent we calculate it as a percentage of the parent"
	aBrick owner ifNil: [ Exception signal: 'My height depends on parent, but it is nil' ].
	^ (aBrick owner innerBounds height * aBrick layout vPercent / 100) ceiling