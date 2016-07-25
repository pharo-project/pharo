lookups
getNewsAuthPassword
	"Return the Password for the authorised news servers"
	"InternetConfiguration getNewsAuthPassword"

	^self primitiveGetStringKeyedBy: 'NewsAuthPassword'
