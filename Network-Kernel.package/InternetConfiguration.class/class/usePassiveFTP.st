tests
usePassiveFTP
	"Return true if UsePassiveFTP"
	"InternetConfiguration usePassiveFTP"

	^(self primitiveGetStringKeyedBy: 'UsePassiveFTP') = '1'
