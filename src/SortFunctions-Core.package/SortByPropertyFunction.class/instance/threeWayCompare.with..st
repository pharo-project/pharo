evaluating
threeWayCompare: anObject1 with: anObject2

	^ self 
		collateProperty: 	(propertyValuable value: anObject1)
		with: (propertyValuable value: anObject2)