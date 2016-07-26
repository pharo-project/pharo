scaling, rotation
shrink: aRectangle by: scale 
	| scalePt |
	scalePt := scale asPoint.
	^ self magnify: aRectangle by: (1.0 / scalePt x asFloat) @ (1.0 / scalePt y asFloat)