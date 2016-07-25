lookups
getGopherHost
	"Return the default Gopher server"
	"InternetConfiguration getGopherHost"

	^self primitiveGetStringKeyedBy: 'GopherHost'
