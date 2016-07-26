operations
beSelection
	"make sure that start and end are distinct objects,
	so when you start moving one, you don't move the other one"
	end == start ifTrue: [ end := start copy ]