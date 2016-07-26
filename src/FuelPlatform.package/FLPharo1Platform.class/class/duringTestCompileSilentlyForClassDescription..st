private-extensions-classdescription
duringTestCompileSilentlyForClassDescription: code
	<flExtensionOf: #ClassDescription selector: #duringTestCompileSilently:>
	^ self 
		duringTestCompileSilently: code 
		storeSource: true