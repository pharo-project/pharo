accessing
addActiveOrigin: aPort 
	^ self originReferences 
		add: 
			((GLMOriginPortReference new) 
				port: aPort; 
				beActive; 
				yourself)