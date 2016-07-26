icons
addedClassIcon
	^ addedClassIcon
		ifNil: [ addedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (Smalltalk ui icons iconNamed: #overlayAdd) ]