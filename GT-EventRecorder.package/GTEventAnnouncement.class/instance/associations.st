converting
associations
	^ dictionary 
			ifNil: [ #() ]
			ifNotNil: [ dictionary associations ]