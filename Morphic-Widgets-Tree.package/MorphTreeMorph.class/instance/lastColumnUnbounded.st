column handling
lastColumnUnbounded
	"Return true if the last column can not be resized with a resizer"
	^ unboundLastColumn ifNil: [unboundLastColumn := true]