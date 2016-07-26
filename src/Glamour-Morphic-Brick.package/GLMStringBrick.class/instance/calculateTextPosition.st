accessing
calculateTextPosition

	self textPosition = #leftCenter ifTrue: [ 
		^ (self brickBounds origin + self textPositionVerticalCenter) truncated
	].

	self textPosition = #center ifTrue: [ 
		^ (self brickBounds origin + self textPositionVerticalCenter + self textPositionHorizontalCenter)floor
	].

	self textPosition = #leftTop ifTrue: [
		^ self brickBounds origin
	].

	self textPosition = #rightCenter ifTrue: [
		^ ((self brickBounds topRight + self textPositionVerticalCenter - (self textSize x@0)) max: self brickBounds origin) floor
	].

	^ self brickBounds origin