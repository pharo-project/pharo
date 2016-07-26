*GT-SpotterExtensions-Core-private
allAncestors
	^ (self allAncestorsTo: OrderedCollection new)
		sort: [ :a :b | a timeStamp >= b timeStamp ]