geometry
extent: aPoint
	"Allow as normal."
	
	self basicExtent: aPoint.
	
	(self layout = #scaled or: [self layout = #scaledAspect]) ifTrue: [self cachedForm: nil]
