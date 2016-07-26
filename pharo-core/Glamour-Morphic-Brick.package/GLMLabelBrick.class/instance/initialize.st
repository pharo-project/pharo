initialization
initialize
	super initialize.
	
	stringBrick := self newStringBrick.
	dotsBrick := self newStringBrick.
	
	self
		hShrinkWrap;
		vShrinkWrap.
	
	stringBrick
			marginRight: self defaultInsetBeforeDots.
	self privateAddBrickBack: stringBrick.
	
	"creating dots morph, that will be displayed when text is being clipped"
	dotsBrick
		text: self defaultDotsText;
		width: #approximateWidth;
		floating: #right;
		zIndex: 2;
		marginTop: 1