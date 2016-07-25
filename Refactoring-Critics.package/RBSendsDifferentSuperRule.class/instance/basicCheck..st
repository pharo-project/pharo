running
basicCheck: aMethod 

	^ aMethod superMessages anySatisfy: [ :message |
		message ~= aMethod selector ]
	
