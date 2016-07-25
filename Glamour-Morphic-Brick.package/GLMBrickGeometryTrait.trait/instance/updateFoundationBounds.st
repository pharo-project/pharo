private-geometry
updateFoundationBounds

	self owner ifNotNil: [ :o | o isFoundationBrick ifTrue: [ 
		self doNotLayoutDuring: [
			(o isVShrinking | o isHShrinking) ifTrue: [ o privateExtent: self outerBounds extent ] ] ] ]