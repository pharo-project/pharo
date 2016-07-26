instance creation
forDefinition: anMCDefinition
	^ anMCDefinition actualClass 
		ifNil: [ IceRemovedClassReference for: anMCDefinition ]
		ifNotNil: [:class | IceSimpleClassReference new targetClass: class ]