lookups
getFTPHost
	"Return the FTPHost"
	"InternetConfiguration getFTPHost"

	^self primitiveGetStringKeyedBy: 'FTPHost'
