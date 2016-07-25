icons
modifiedClassIcon
	^ modifiedClassIcon
		ifNil: [ modifiedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (Smalltalk ui icons iconNamed: #overlayModification) ]