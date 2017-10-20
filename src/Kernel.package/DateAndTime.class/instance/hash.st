comparing
hash
	^ (julianDayNumber hashMultiply bitXor: seconds) bitXor: nanos