accessing
addPassiveOrigin: aPort
	^ self originReferences 
		add: 
			((GLMOriginPortReference new) 
				port: aPort; 
				bePassive; 
				yourself)