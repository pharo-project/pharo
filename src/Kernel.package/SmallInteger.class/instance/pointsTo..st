pointers
pointsTo: anObject
	"Answers true if I hold a reference to anObject, or false otherwise. But since SmallIntegers do not have pointers, it always answers false"
	^false
