geometry
extent: aPoint
	"Update the bar fillStyle if appropriate."
	
	super extent: aPoint.
	self fillStyle isOrientedFill ifTrue: [
		self fillStyle: (self theme progressBarFillStyleFor: self)].
	self barFillStyle isOrientedFill ifTrue: [
		self barFillStyle: ( self theme progressBarProgressFillStyleFor: self)]