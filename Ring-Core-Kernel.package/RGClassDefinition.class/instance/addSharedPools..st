shared pools
addSharedPools: aCollection
	aCollection do: [ :pool | self addSharedPoolNamed: pool ]