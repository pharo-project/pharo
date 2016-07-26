accessing
subtype
	"Return the subType of the platform we're running on"

	^ (Smalltalk vm getSystemAttribute: 1003)