initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	
	smoothing := 1.
	transform := MorphicTransform identity