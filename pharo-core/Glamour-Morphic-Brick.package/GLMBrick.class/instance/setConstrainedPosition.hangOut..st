morph-common
setConstrainedPosition: aPoint hangOut: partiallyOutside
	"Change the position of this morph and and all of its submorphs to aPoint, but don't let me go outside my owner's bounds."

	| trialRect delta boundingRectangle |
	self owner ifNil:[ ^ self ].
	trialRect := 0@0 extent: self outerBounds extent.
	boundingRectangle := (self owner paddingLeft @ self owner paddingTop) extent: self owner innerBounds extent.
	delta := (((aPoint + trialRect corner) min: (boundingRectangle corner)) - trialRect corner) max: boundingRectangle origin.
	
	self position: delta.
	self changed
