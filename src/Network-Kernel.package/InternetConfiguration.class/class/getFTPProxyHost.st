lookups
getFTPProxyHost
	"Return the FTP proxy host"
	"InternetConfiguration getFTPProxyHost"

	^self primitiveGetStringKeyedBy: 'FTPProxyHost'
