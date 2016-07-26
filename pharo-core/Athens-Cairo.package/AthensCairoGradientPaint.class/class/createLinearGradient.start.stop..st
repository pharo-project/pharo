as yet unclassified
createLinearGradient: aColorRamp start: aStartPoint stop: aStopPoint
	| paint |
	paint := self
		primCreateLinearX0: aStartPoint x asFloat
		y0: aStartPoint y asFloat
		x1: aStopPoint x asFloat
		y1: aStopPoint y asFloat.
	
	"note, we do #initialize here because instance was created by primitive"
	paint initialize; populateRamp: aColorRamp.
	^ paint
