instance creation
major: major minor: minor
	"Create a new instance of the receiver with the given major minor number.

		SystemVersion major: 6 minor: 1 
	"

	^self major: major minor: minor suffix: ''