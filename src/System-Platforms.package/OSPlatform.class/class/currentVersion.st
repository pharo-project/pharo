system attributes
currentVersion
	"Return the version number string of the platform we're running on"

	^ (Smalltalk vm getSystemAttribute: 1002) asString