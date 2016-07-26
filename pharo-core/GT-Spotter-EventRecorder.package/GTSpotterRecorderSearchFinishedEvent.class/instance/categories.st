accessing
categories
	^ self processors 
			ifNotNil: [ :aCollection | aCollection select: #isString ]
			ifNil: [ #() ]