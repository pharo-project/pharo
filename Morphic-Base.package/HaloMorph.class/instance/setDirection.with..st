private
setDirection: anEvent with: directionHandle
	"The user has let up after having dragged the direction arrow; now set the forward direction of the actual SketchMorph accordingly"
	anEvent hand obtainHalo: self.
	target setDirectionFrom: directionHandle center.
	self endInteraction