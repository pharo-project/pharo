rectangle functions
amountToTranslateWithin: aRectangle 
	"Answer a Point, delta, such that self + delta is forced within aRectangle.
	Keep self topLeft inside when all of self cannot be made to fit"
	| dx dy |
	dx := 0.
	dy := 0.
	self right > aRectangle right ifTrue: [ dx := aRectangle right - self right ].
	self bottom > aRectangle bottom ifTrue: [ dy := aRectangle bottom - self bottom ].
	self left + dx < aRectangle left ifTrue: [ dx := aRectangle left - self left ].
	self top + dy < aRectangle top ifTrue: [ dy := aRectangle top - self top ].
	^ dx @ dy