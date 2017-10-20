error handling
handle: anException about: anEntity forProppertiesDo: aBlock

	aBlock value: (
		ReExceptionProperty
			for: anEntity
			with: anException)
	
	