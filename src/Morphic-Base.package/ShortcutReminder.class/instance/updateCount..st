private
updateCount: aHashValue
	
	| value |
	
	value := countDict at: aHashValue ifAbsent: [ 0 ].
	
	countDict at: aHashValue put: value + 1.