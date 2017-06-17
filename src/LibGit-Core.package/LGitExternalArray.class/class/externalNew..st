instance creation
externalNew: numberOfElements
	^ self 
		externalNewType: self type 
		size: numberOfElements