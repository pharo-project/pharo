lookups
getPhHost
	"Return the PhHost server"
	"InternetConfiguration getPhHost"

	^self primitiveGetStringKeyedBy: 'PhHost'
