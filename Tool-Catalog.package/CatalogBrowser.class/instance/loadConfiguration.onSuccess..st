actions
loadConfiguration: aProject onSuccess: aBlock
	[ 
		aProject loadConfiguration.
		aBlock value ] 
	on: Error do: [ :e | 
		e logCr.
		self inform: 'There was an error while trying to load ', aProject name, '. Loading was cancelled.' ]