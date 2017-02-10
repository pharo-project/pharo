testing
testCounterRollover
	| previousUuid uuid |
	previousUuid := generator next.
	"the internal counter inside generator will rollover at 16r10000,
	the least significant 10th byte rolls over every 16r100,
	the internal counter starts at a random value"
	2 * 16r10000 timesRepeat: [ 
		uuid := generator next.
		"same generator, consequetive counters (take roll-over into account)"
		self assert: ((previousUuid at: 10) + 1) \\ 256 equals: (uuid at: 10).
		previousUuid := uuid ]