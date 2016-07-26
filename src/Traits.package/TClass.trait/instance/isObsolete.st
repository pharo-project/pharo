testing
isObsolete
	"Return true if the receiver is obsolete."

	^ self propertyAt: #obsolete ifAbsent: false