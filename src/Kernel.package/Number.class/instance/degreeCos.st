mathematical functions
degreeCos
	"Answer the cosine of the receiver taken as an angle in degrees."
	
	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin