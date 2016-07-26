accessing
labelMorphFor: aContainer
	^ self settingDeclaration 
		ifNotNil: [:v | v labelMorphFor: aContainer]