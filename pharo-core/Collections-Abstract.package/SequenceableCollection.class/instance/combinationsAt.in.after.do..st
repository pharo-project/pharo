private
combinationsAt: jj in: aCollection after: nn do: aBlock
	"Choose k of N items and put in aCollection.  jj-1 already chosen.  Indexes of items are in numerical order, to avoid the same combo being used twice.  In this slot, we are allowed to use items in self indexed by nn+1 to self size.  nn is the index used for position jj-1."

	nn + 1 to: self size do: [ :index | 
		aCollection at: jj put: (self at: index).
		jj = aCollection size
			ifTrue: [ aBlock value: aCollection ]
			ifFalse: [ 
				self
					combinationsAt: jj + 1
					in: aCollection
					after: index
					do: aBlock ] ]