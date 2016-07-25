api
initialExtent

	^ initialExtent value
		ifNil: [ self model 
			ifNil: [ self defaultInitialExtent ]
			ifNotNil: [:m | m extent
				ifNil: [ m initialExtent
					ifNil: [ self defaultInitialExtent ] ] ] ]