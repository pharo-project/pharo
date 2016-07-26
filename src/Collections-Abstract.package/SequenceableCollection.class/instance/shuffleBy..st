shuffling
shuffleBy: aRandom
	"Durstenfeld's version of the Fisher-Yates shuffle"

	self size to: 2 by: -1 do: [ :i | 
		self swap: i with: (aRandom nextInt: i) ]