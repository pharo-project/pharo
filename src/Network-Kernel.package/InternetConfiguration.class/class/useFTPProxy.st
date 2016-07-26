tests
useFTPProxy
	"Return true if UseFTPProxy"
	"InternetConfiguration useFTPProxy"

	^(self primitiveGetStringKeyedBy: 'UseFTPProxy') = '1'
