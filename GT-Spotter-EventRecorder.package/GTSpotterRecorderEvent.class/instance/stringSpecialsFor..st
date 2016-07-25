accessing
stringSpecialsFor: anArrayOrNil
	^ anArrayOrNil 
			ifNil: [ #() ]
			ifNotNil: [ :array | array third ]