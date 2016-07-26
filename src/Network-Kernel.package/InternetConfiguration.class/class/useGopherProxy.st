tests
useGopherProxy
	"Return true if UseGopherProxy"
	"InternetConfiguration useGopherProxy"

	^(self primitiveGetStringKeyedBy: 'UseGopherProxy') = '1'
