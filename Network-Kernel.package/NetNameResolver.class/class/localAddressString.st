lookups
localAddressString
	"Return a string representing the local host address as four decimal bytes delimited with decimal points."
	"NetNameResolver localAddressString"

	^ NetNameResolver stringFromAddress: NetNameResolver localHostAddress
