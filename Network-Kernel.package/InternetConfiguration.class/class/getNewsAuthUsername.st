lookups
getNewsAuthUsername
	"Return the user name for authorised news servers"
	"InternetConfiguration getNewsAuthUsername"

	^self primitiveGetStringKeyedBy: 'NewsAuthUsername'
