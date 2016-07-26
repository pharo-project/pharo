instance creation
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ self withAll: aCollection

"Examples:
	Bag newFrom: {1. 2. 3. 3}
	{1. 2. 3. 3} as: Bag
"