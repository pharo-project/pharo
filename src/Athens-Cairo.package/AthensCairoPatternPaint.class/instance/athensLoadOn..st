drawing
athensLoadOn: anAthensCanvas 
	"
	Note: The pattern's transformation matrix will be locked to the user space in effect at the time of cairo_set_source(). This means that further modifications of the current transformation matrix will not affect the source pattern. See cairo_pattern_set_matrix().
	"

	self isSuccess
		ifTrue:[ anAthensCanvas primSetSource: self ]
		ifFalse:[ anAthensCanvas setSourceR: 1.0 g: 0.0 b: 0.0 a: 1.0].
	 
		
