accessing
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Only lock additions, removals executing in parallel would have little effect on the resulting array"

	ProtectAdd critical: [
		self unprotectedExternalObjects: Array new].
