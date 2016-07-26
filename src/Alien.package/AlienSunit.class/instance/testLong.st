testing
testLong
	| long unsignedIntegerIntervalue k signedIntegerIntervalue |
	
	long := Alien newC: 4.
	#( 0 4294967295) do: 
		[:i  | long unsignedLongAt: 1 put: i.
		self should: [(long unsignedLongAt: 1) = i]].
	#( -2147483648 2147483647) do: 
		[:j |  long signedLongAt: 1 put: j.
		self should: [(long signedLongAt: 1) = j]].
	unsignedIntegerIntervalue := (0 to: 4294967295).
	10000 timesRepeat: [
			k := unsignedIntegerIntervalue atRandom.
			long unsignedLongAt: 1 put: k.
			self should: [(long unsignedLongAt: 1) = k]].
	signedIntegerIntervalue := (-2147483648 to: 2147483647).
	10000 timesRepeat: [
			k := signedIntegerIntervalue atRandom.
			long signedLongAt: 1 put: k.
			self should: [(long signedLongAt: 1) = k]].
	
	signedIntegerIntervalue := (-4294967295 to: -2147483649).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [long signedLongAt: 1 put: k] raise: Error].
	
	signedIntegerIntervalue := (2147483648 to: 4294967295).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [long signedLongAt: 1 put: k] raise: Error].
	