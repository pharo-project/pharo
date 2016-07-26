private
updateCache
	|left top right bottom |
	
	left := self brickBounds origin x.
	top := self brickBounds origin y.
	right := self brickBounds corner x.
	bottom := self brickBounds corner y.
	
	innerBounds := ((left + self paddingLeft)@(top + self paddingTop)) corner: ((right - self paddingRight)@(bottom - self paddingBottom)).
	outerBounds := ((left - self marginLeft)@(top - self marginTop)) corner: ((right + self marginRight)@(bottom + self marginBottom)).