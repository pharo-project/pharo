instance creation
instantiateModels: aCollectionOfPairs
 
	aCollectionOfPairs pairsDo: [ :k :v |
		widgets at: k asSymbol put: (self createInstanceFor: v) ]