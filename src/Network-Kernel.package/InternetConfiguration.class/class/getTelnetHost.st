lookups
getTelnetHost
	"Return the TelnetHost server"
	"InternetConfiguration getTelnetHost"

	^self primitiveGetStringKeyedBy: 'TelnetHost'
