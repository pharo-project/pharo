private
computeCheckTopLeft
	| center offset |
	
	center := self checkRectangle center.
	offset := (self checkWidth / 2.0) truncated.
	^ (center x - offset) @ (center y - offset - 1)