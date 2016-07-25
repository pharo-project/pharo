tests
useHTTPProxy
	"Return true if UseHTTPProxy"
	"InternetConfiguration useHTTPProxy"

	^(self primitiveGetStringKeyedBy: 'UseHTTPProxy') = '1'
