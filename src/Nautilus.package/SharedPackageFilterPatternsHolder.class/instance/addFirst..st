adding
addFirst: aString 
	^ mutex critical: [ 
		value 
			detect: [ :eachPattern | aString beginsWith: eachPattern ]
			ifFound: [ :aPattern | value remove: aPattern ].
		value addFirst: aString.
	 ]