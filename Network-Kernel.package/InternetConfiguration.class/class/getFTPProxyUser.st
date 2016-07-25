lookups
getFTPProxyUser
	"Return the first level FTP proxy authorisation"
	"InternetConfiguration getFTPProxyUser"

	^self primitiveGetStringKeyedBy: 'FTPProxyUser'
