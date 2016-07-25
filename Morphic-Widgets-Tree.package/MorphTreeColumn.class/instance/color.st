column drawing
color 
	^ color ifNil: [self container columnColors at: ((self index \\ 2) + 1)]