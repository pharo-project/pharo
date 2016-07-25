lookups
getFTPProxyAccount
	"Return the second level FTP proxy authorisation"
	"InternetConfiguration getFTPProxyAccount"

	^self primitiveGetStringKeyedBy: 'FTPProxyAccount'
