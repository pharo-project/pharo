lookups
getFingerHost
	"Return the default finger server"
	"InternetConfiguration getFingerHost"

	^self primitiveGetStringKeyedBy: 'FingerHost'
