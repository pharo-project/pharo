private
nextRandom16
	"Return the next 16-bit random value from my random generator"

	^ (random nextInt: 16r10000) - 1