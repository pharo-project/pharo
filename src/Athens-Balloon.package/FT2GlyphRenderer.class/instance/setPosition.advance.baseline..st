private
setPosition: aPoint advance: advancePt baseline: baselineOffset
	"aPoint is a text origin in user's coordinate system,
	and andvance is accumulated advance came from another renderer instance, expressed in surface's coordinate system"

	| pt offset |
	
	offset :=  baselineOffset - font getPreciseAscent "(font face ascender  * pixelSize / font face unitsPerEm)".

	pt := aPoint + (0@offset).
	pt := surface pathTransform transform: pt.
	origin := pt.
	pt := pt + advancePt.
	
	"remember the origin, so on #getAdvance , we will answer the current advance for renderer in surface coordinate system"
	
"	2 raisedTo: 6 "
	penX := (pt x * 64) rounded.
	penY := (pt y * 64) rounded.

