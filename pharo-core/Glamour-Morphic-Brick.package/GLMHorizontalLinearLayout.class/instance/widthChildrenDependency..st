width
widthChildrenDependency: aBrick
	"returns the width of aBrick when it depends on children"
	^ aBrick paddingLeft + aBrick paddingRight + ((self nonFloatSubbricks: aBrick) detectSum: [ :each | each outerBounds width ])