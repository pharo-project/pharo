converting
associations
	^ info 
			ifNil: [ #() ]
			ifNotNil: [ info associations ]