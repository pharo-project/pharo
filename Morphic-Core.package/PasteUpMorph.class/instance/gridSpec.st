gridding
gridSpec
	"Gridding rectangle provides origin and modulus"

	^ self valueOfProperty: #gridSpec ifAbsent: [0@0 extent: 8@8]