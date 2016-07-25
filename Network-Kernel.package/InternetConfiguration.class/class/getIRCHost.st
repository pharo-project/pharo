lookups
getIRCHost
	"Return the Internet Relay Chat server"
	"InternetConfiguration getIRCHost"

	^self primitiveGetStringKeyedBy: 'IRCHost'
