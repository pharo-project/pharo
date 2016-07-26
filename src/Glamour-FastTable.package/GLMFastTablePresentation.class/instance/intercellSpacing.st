public api
intercellSpacing
	^ self parameters at: 'intercellSpacing' ifAbsent: [ 8@1 ]