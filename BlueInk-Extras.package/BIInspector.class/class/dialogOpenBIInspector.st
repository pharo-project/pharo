settings
dialogOpenBIInspector
	^ Smalltalk ui theme
		newRowIn: World
		for:
			{(Smalltalk ui theme buttonLabelForText: 'Open Blue Ink Inspector ' translated).
			self buildOpenBIInspectorButton}