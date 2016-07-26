height
heightChildrenDependency: aBrick
	"returns the height of aBrick when it depends on children"
	"in case of horizontal linear layout it is the height of the highest subbrick + corresponding paddings"
	^ aBrick paddingTop + aBrick paddingBottom + (((self nonFloatSubbricks: aBrick) detectMax: [ :each | each outerBounds height ])
		ifNil: [ 0 ]
		ifNotNil: [ :m | m outerBounds height ])