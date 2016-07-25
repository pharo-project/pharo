private
hash
	^ (julianDayNumber hashMultiply bitXor: seconds) bitXor: nanos