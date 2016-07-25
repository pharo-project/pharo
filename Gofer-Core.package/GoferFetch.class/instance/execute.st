running
execute
	self model
		do: [ :reference | self cacheRepository storeVersion: reference version ]
		displayingProgress: 'Fetching Versions'