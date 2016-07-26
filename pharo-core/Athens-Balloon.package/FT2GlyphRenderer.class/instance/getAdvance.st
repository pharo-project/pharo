accessing
getAdvance
	"aPoint is a text origin in user's coordinate system,
	and anvance is accumulated advance came from another renderer instance, expressed in surface's coordinate system"

	| pt |
	pt := (penX/64.0) @ (penY/64.0) - origin.

	^ pt