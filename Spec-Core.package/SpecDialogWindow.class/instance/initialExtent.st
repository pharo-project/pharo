open/close
initialExtent 

	^ self model
		ifNil: [ super initialExtent ]
		ifNotNil: [:m | m initialExtent
						ifNil: [ super initialExtent ]]