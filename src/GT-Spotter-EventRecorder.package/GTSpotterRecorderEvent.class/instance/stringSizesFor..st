accessing
stringSizesFor: anArrayOrNil
	^ anArrayOrNil 
			ifNil: [ #() ]
			ifNotNil: [ :array | array second ]