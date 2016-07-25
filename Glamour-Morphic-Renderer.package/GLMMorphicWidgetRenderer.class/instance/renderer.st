accessing
renderer
	^ renderer ifNil: [renderer := GLMMorphicRenderer new]