testing
testAtAddress
	| alien address alias |
	alien := Alien newCString: 'foo'.
	address := alien address.
	"This means alias and alien both point to the same address"
	alias := Alien atAddress: address dataSize: 4.
	self should: [alias dataSize = 4].
	self should: [alias strcpy = 'foo'].
	alien free.