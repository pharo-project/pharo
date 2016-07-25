address string utils
stringFromAddress: addr
	"Return a string representing the given host address as four decimal bytes delimited with decimal points."
	"NetNameResolver stringFromAddress: NetNameResolver localHostAddress"

	| s |
	s := String new writeStream.
	1 to: 3 do: [ :i | (addr at: i) printOn: s. s nextPut: $.].
	(addr at: 4) printOn: s.
	^ s contents
