lookups
getWAISGateway
	"Return the wais gateway"
	"InternetConfiguration getWAISGateway"

	^self primitiveGetStringKeyedBy: 'WAISGateway'
