instance creation
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^newCollection

"	OrderedCollection newFrom: {1. 2. 3}
	{1. 2. 3} as: OrderedCollection
	{4. 2. 7} as: SortedCollection
"