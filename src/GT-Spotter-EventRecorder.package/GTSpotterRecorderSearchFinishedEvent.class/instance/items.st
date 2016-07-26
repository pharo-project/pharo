accessing
items
	^ self processors 
			ifNotNil: [ :aCollection | aCollection select: #isNumber ]
			ifNil: [ #() ]