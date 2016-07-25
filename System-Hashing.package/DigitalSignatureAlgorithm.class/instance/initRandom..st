initialization
initRandom: randomInteger
	"Initialize the the secure random number generator with the given value. The argument should be a positive integer of up to 512 bits chosen randomly to avoid someone being able to predict the sequence of random values generated."
	"Note: The random generator must be initialized before generating a key set or signature. Signature verification does not require initialization of the random generator."

	randSeed := 16rEFCDAB8998BADCFE10325476C3D2E1F067452301.  "initial seed"
	randKey := randomInteger.
	Transcript show: 'Random seed: ', randomInteger printString; cr.
