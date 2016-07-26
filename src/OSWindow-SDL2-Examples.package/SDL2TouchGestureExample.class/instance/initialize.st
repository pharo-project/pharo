initialization
initialize
	super initialize.
	circleCenter := (self defaultExtent x / 2) asFloat @ (self defaultExtent y / 2) asFloat.
	circleSize := 10.0