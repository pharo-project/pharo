cubic support
calcEnoughSegments
	"Find the power of two that represents a sufficient number of  segments for this cubic.  
	The measure is the sum of distances for the segments.  
	We want this to be close enough not affect the straightness of  
	the drawn lines. Which means within one pixel."
	
	^ self
		enough: 4
		withMeasure: (self measureFor: 2)
		withIn: self leeway