accessing
stringHashesFor: anArrayOrNil
	^ anArrayOrNil 
			ifNil: [ #() ]
			ifNotNil: [ :array | array first ]