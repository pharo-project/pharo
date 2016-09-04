icons
addedClassIcon
	^ addedClassIcon
		ifNil: [ addedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayAdd) ]