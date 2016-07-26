private-extensions-classdescription
duringTestCompileSilentlyForClassDescription: code storeSource: storeSource
	<flExtensionOf: #ClassDescription selector: #duringTestCompileSilently:storeSource:>
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: ''