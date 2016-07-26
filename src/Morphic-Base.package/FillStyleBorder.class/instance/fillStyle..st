accessing
fillStyle: anObject
	"Set the value of fillStyle"

	fillStyle := anObject.
	anObject ifNotNil: [self baseColor: anObject asColor]