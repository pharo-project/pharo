lookups
getWhoisHost
	"Return the WhoisHost server"
	"InternetConfiguration getWhoisHost"

	^self primitiveGetStringKeyedBy: 'WhoisHost'
