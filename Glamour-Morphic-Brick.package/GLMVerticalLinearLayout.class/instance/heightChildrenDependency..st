height
heightChildrenDependency: aBrick
	"returns the height of aBrick when it depends on children"
	^ aBrick paddingTop + aBrick paddingBottom + ((self nonFloatSubbricks: aBrick) detectSum: [ :each | each outerBounds height ])