lookups
getGopherProxy
	"Return the  Gopher proxy"
	"InternetConfiguration getGopherProxy"

	^self primitiveGetStringKeyedBy: 'GopherProxy'
