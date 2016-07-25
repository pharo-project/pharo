initialization
initialize
	"DigitalSignatureAlgorithm initialize"

	"SmallPrimes is a list of small primes greater than two."
	SmallPrimes := Integer primesUpTo: 2000.
	SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size.

	"HighBitOfByte maps a byte to the index of its top non-zero bit."
	HighBitOfByte := (0 to: 255) collect: [:byte | byte highBit].
