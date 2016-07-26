shared pools
allSharedPoolNames
	^ self allSharedPools collect: [ :pool | pool name ]