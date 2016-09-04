icons
modifiedClassIcon
	^ modifiedClassIcon
		ifNil: [ modifiedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayModification) ]