scripting
intercellSpacing
	^ self parameters 
		at: 'intercellSpacing' 
		ifAbsent: [ self class defaultIntercellSpacing ]