icons
deletedClassIcon
	^ deletedClassIcon
		ifNil: [ deletedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayRemove) ]