tests
useSocks
	"Return true if UseSocks"
	"InternetConfiguration useSocks"

	^(self primitiveGetStringKeyedBy: 'UseSocks') = '1'
