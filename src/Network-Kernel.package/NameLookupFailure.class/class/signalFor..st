instance creation
signalFor: hostName
	"Create and signal a NameLookupFailure for hostName"

	^ (self hostName: hostName) signal