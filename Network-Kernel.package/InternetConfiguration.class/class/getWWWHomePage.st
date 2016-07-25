lookups
getWWWHomePage
	"Return the WWW home page url"
	"InternetConfiguration getWWWHomePage"

	^self primitiveGetStringKeyedBy: 'WWWHomePage'
