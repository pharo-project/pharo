accessing
firstOrNil
	^ mutex critical: [ 
		value 
			ifNotEmpty: [ value first ] 
			ifEmpty: [ nil ] ]