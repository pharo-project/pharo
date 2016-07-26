lookups
getNNTPHost
	"Return the NNTP server"
	"InternetConfiguration getNNTPHost"

	^self primitiveGetStringKeyedBy: 'NNTPHost'
