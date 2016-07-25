drop shadows
hasDropShadow
	"answer whether the receiver has DropShadow"
	^ self
		valueOfProperty: #hasDropShadow
		ifAbsent: [false]