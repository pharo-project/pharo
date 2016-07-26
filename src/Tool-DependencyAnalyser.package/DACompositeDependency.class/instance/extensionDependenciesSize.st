accessing
extensionDependenciesSize
	^ self dependencies
		detectSum: [ :dependency | 
			dependency isExtensionDependency
				ifTrue: [ 1 ]
				ifFalse: [ 0 ] ]