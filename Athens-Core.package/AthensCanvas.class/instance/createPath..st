paths
createPath: aPathCreatingBlock
	"Create new path. A path creating block should be a monadic block,
	which will get a path builder instance as argument. See AthensPathBuilder
	for available protocol.
	The resulting path object can be later used for drawing.
	" 
	^ surface createPath: aPathCreatingBlock