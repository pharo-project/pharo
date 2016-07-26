lookups
getSocksHost
	"Return the Socks server"
	"InternetConfiguration getSocksHost"

	^self primitiveGetStringKeyedBy: 'SocksHost'
