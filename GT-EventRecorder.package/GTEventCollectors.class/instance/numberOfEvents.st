accessing
numberOfEvents
	^ mutex critical: [ 
		collectors 
			ifEmpty: 0
			ifNotEmpty: [ collectors sum: #size ] ]