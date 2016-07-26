lookups
getNTPHost
	"Return the  Network Time Protocol (NTP)"
	"InternetConfiguration getNTPHost"

	^self primitiveGetStringKeyedBy: 'NTPHost'
