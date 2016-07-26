width
widthChildrenDependency: aBrick
	"returns the height of aBrick when it depends on children"
	"in case of horizontal linear layout it is the height of the highest subbrick + corresponding paddings"
	^ aBrick paddingLeft + aBrick paddingRight + (((self nonFloatSubbricks: aBrick) detectMax: [ :each | each outerBounds width ])
		ifNil: [ 0 ]
		ifNotNil: [ :m | m outerBounds width ])