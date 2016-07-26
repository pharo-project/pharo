geometry
position: aPoint
	"Prevent moving a world (e.g. via HandMorph>>specialGesture:)"

	super position: aPoint.
	self viewBox ifNotNil: [:viewBox | self viewBox: (aPoint extent:  viewBox extent)].

