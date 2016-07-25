accessing-path
addPathSegments: pathSegments
	"Modify the receiver's path by adding the elements of pathSegments at the end"

	pathSegments do: [ :each | self addPathSegment: each ]