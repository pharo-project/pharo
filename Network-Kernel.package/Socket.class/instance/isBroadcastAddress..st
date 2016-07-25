queries
isBroadcastAddress: anAddress
	"Only IPv4 has broadcast, in IPv6 all nodes are required to support multicast, and that is used instead."

	^ anAddress size = 4 and: [ anAddress last = 255 ]