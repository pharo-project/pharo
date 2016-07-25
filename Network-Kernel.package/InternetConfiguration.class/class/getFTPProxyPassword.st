lookups
getFTPProxyPassword
	"Return the FTP proxy password"
	"InternetConfiguration getFTPProxyPassword"

	^self primitiveGetStringKeyedBy: 'FTPProxyPassword'
