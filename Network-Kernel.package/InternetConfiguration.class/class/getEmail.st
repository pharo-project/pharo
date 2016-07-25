lookups
getEmail
	"Return the  email address of user"
	"InternetConfiguration getEmail"

	^self primitiveGetStringKeyedBy: 'Email'
