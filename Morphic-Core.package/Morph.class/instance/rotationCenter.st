rotate scale and flex
rotationCenter
	"Return the rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	^self valueOfProperty: #rotationCenter ifAbsent: [0.5@0.5]
