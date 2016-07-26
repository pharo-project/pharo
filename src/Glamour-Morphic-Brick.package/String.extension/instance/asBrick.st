*Glamour-Morphic-Brick
asBrick

	^ GLMLabelBrick new
		text: self asString;
		height: [ :b | b approximateHeight ]