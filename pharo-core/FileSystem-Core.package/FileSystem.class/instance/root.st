accessing
root
	"Returns the root of the receiver filesystem, i.e. / on unix"
	^ self referenceTo: Path root