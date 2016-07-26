private
numValuesPerPower
	"Answer the number of values that should be available in the cached table of primes
	for each power-of-two range. A larger number allows closer sizing for pre-sized collections,
	but results in a larger table that takes longer to search."
	
	^5 "Chosen so there will be fewer than 128 primes in the sizes table"