queries
wantsToShow: aHashValue
	
	^limit < 0 or: [ 
		(countDict at: aHashValue ifAbsent: [ 0 ]) < limit
	]